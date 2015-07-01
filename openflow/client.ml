(*
 * Testing if the of library does the job for us
 * *)
open Lwt
open Lwt_io
open Lwt_bytes
open OpenFlow0x04 
open OpenFlow0x04_Core 
open OpenFlow0x04.Message

let of_port = 6633

type t = {
    istream: Lwt_io.input_channel;
    ostream: Lwt_io.output_channel;
    wait : unit Lwt.t;
    wake : unit Lwt.u;
    resp : (int32, Message.t Lwt.u) Hashtbl.t;
}

let send_of_msg_reply c xid m = 
    let buf = Message.marshal xid m in
    let (t, u) = Lwt.wait () in 
    let _ = Hashtbl.add c.resp xid u in
    lwt _ = Lwt_io.write_from_string_exactly c.ostream 
                buf 0 (String.length buf) in
    lwt r = t in
    let _ = Hashtbl.remove c.resp xid in
    return r


let send_of_msg c xid m = 
    let buf = Message.marshal xid m in
    Lwt_io.write_from_string_exactly c.ostream 
                buf 0 (String.length buf) 

let proccess_msg c xid = function
    | Hello t -> 
            Printf.printf "> (%lu) Hello\n%!" xid;
            let u = Hashtbl.find c.resp 100l in
            let _ = Lwt.wakeup u (Hello t) in
            let _ = Hashtbl.remove c.resp 100l in
            send_of_msg c xid FeaturesRequest
    | EchoRequest _ -> 
            Printf.printf "> (%lu) EchoRequest\n%!" xid;
            send_of_msg c xid (EchoReply (Cstruct.create 0))
    | m ->
            let _ = 
                if (Hashtbl.mem c.resp xid) then
                    let _ = Printf.printf "found xid %ld hander\n%!" xid in
                    let _ = Lwt.wakeup (Hashtbl.find c.resp xid) m in 
                    Hashtbl.remove c.resp xid
                else ()
            in
            return (Printf.printf "> %s\n%!" (Message.to_string m))


let of_loop c process_msg =
    let buf = Bytes.create 2048 in 
    let rec loop_inner () =
        lwt _ = read_into_exactly c.istream buf 0 (OpenFlow_Header.size) in
        let h = OpenFlow_Header.parse 
        (Cstruct.of_string 
            (Bytes. to_string (Bytes.sub buf 
            0 (OpenFlow_Header.size)))) in
        lwt (xid, m) = 
            match (h.OpenFlow_Header.length - OpenFlow_Header.size) with
            | 0 -> return (OpenFlow0x04.Message.parse h (String.make 0 '\000'))
            | len ->
                    lwt _ = read_into_exactly c.istream buf 0 len in
                    let msg = Bytes. to_string (Bytes.sub buf 0 len) in
                    return (OpenFlow0x04.Message.parse h msg)
        in
        lwt _ = process_msg c xid m in 
            loop_inner ()
    in
    lwt _ = loop_inner () in 
    return (Lwt.wakeup c.wake ())


let wait c = c.wait

let create_controller () = 
        let sockaddr = Unix.ADDR_INET((Unix.inet_addr_of_string "127.0.0.1"), of_port) in
        lwt (istream, ostream) = Lwt_io.open_connection sockaddr in
        let (wait, wake) = Lwt.wait () in 
        let c = {istream;ostream;wait; wake;resp=(Hashtbl.create 64);} in
        let (t,u) = Lwt.wait () in
        let _ = Hashtbl.add c.resp 100l u in
        lwt _ = send_of_msg c 100l (Hello []) in
        let _ = Lwt.ignore_result (of_loop c proccess_msg) in 
        lwt _ = t in
        return c

let int64_of_macaddr mac =
    let iMac = ref 0L in
    let _ = String.iter (fun x -> 
        iMac := Int64.add (Int64.shift_left !iMac 8) (Int64.of_int (int_of_char x))) 
        (Macaddr.to_bytes mac) in
    !iMac

let get_port c domid =  
    let vif_name = Printf.sprintf "eth%d" domid in
    let vif_id = String.make 16 '\000' in
    let () = String.blit vif_name 0 vif_id 0 (String.length vif_name) in

    lwt ports = 
        match_lwt (send_of_msg_reply c 101l (MultipartReq ({mpr_type=PortsDescReq;mpr_flags=false;}))) with
        | MultipartReply t -> begin 
                match t.mpreply_typ with
                | PortsDescReply ports -> return ports
                | _ -> return []
                end
        | _ -> return []
    in
    return (List.find (fun (x : PortDesc.t) -> x.name = vif_id) ports )

let get_group_stats c appid = 
    match_lwt (send_of_msg_reply c 105l (MultipartReq ({mpr_type=(GroupStatsReq appid);mpr_flags=false;}))) with 
    | MultipartReply p -> begin
        match p.mpreply_typ with
        | GroupStatsReply r -> return (Some r)
        | _ -> return None
    end 
    | _ -> return None

let get_group_buckets c appid = 
    match_lwt (send_of_msg_reply c 105l (MultipartReq ({mpr_type=GroupDescReq;mpr_flags=false;}))) with
    | MultipartReply r -> begin 
        match r.mpreply_typ with
        | GroupDescReply groups -> 
            let groupDesc = List.find (fun (x:groupDesc) -> x.group_id = appid) groups  in
            return (groupDesc.bucket)
        | _ -> failwith "Group not found"
    end
    | _ -> failwith "Group not found"

let send_flow_mod c mfOfp_match mfInstructions = 
    send_of_msg c 102l  (FlowModMsg  
        {mfCookie={m_value=0L; m_mask=None}; 
        mfTable_id=0;
        mfCommand=AddFlow;
        mfIdle_timeout=(ExpiresAfter 86400);
        mfHard_timeout=Permanent;mfPriority=2;
        mfBuffer_id=None;
        mfOut_port=None;
        mfOut_group=None;
        mfFlags={fmf_send_flow_rem=false;fmf_check_overlap=false;
            fmf_reset_counts=false;fmf_no_pkt_counts=false;fmf_no_byt_counts=false;};
        mfOfp_match;
        mfInstructions;}) 

let add_vm c ip mac port = 
    (* Add the default forwarding rules *)
    lwt _ = send_flow_mod c
                [(OxmInPort port.port_no);]
                [(ApplyActions [(Output (PhysicalPort 2l))])]
    in

    lwt _ = send_flow_mod c
                [(OxmEthType 0x0806);(OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;});]
                [(ApplyActions [(Output (PhysicalPort port.port_no))])]
    in
    lwt _ = send_flow_mod c
                [(OxmEthType 0x0800);(OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
                [(ApplyActions [(Output (PhysicalPort port.port_no))])]
    in
    lwt _ = send_flow_mod c
                [(OxmEthType 0x0806);(OxmARPTpa {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
                [(ApplyActions [(Output (PhysicalPort port.port_no))])]
    in
    return ()

let add_app_of13 c appid ip mac domid sip =
    (* First get the port num for the specific domid *)
    lwt port = get_port c domid in
    let () = Printf.printf "id=%ld\n%!" port.port_no in

    lwt () = add_vm c ip mac port in

    (* Add the new group entry *)
    lwt () = 
        match_lwt get_group_stats c appid with 
        | None -> 
            send_of_msg c 102l  (GroupModMsg (AddGroup (Select, appid, 
                [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
                bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                    (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                    (Output (PhysicalPort port.port_no))]
                }]))) 
        | Some _ ->
            send_of_msg c 102l  (GroupModMsg (ModifyGroup (Select, appid, 
                [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
                bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                    (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                (Output (PhysicalPort port.port_no))]}]))) 
    in 

    (* Add the forwarding rule for the service port number *)
    lwt _ = send_flow_mod c
                [(OxmEthType 0x0800);(OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
                [(ApplyActions [(Group appid)])]
    in
    
    (* Add the inverse path *)
    lwt _ = send_flow_mod c [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
                (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
        [(ApplyActions [(SetField (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 sip);m_mask=None;})); 
        (Output (PhysicalPort port.port_no))])] 
    in

    return ()

let add_replica_of13 src_c dst_c appid ip mac domid sip =
    (* First get the port num for the specific domid *)
    lwt port = get_port dst_c domid in
    let () = Printf.printf "id=%ld\n%!" port.port_no in

    lwt () = add_vm dst_c ip mac port in
    lwt buckets = get_group_buckets src_c appid in

    let sip =
        match (List.nth (List.hd buckets).bu_actions 0) with 
        | SetField r -> begin
            match r with
            | OxmIP4Dst x -> x.m_value
            | _ -> failwith "can't find sip"
        end
        | _ ->  failwith "can't find sip"
    in
    let smac =
        match (List.nth (List.hd buckets).bu_actions 1) with 
        | SetField r -> begin
            match r with
            | OxmEthDst x -> x.m_value
            | _ -> failwith "can't find smac"
        end
        | _ ->  failwith "can't find smac"
    in

    lwt _ = 
        if (src_c == dst_c) then
            send_of_msg src_c 102l  (GroupModMsg (ModifyGroup (Select, appid, 
                buckets @
                [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
                bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                    (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                (Output (PhysicalPort port.port_no))]}])))
            else 
        return ()
    in
    return ()

(*
 * {
        "method":"monitor","id":0,"params":
            ["Open_vSwitch",null,
                {"Port":{"columns":["fake_bridge","interfaces","name","tag"]},
                "Interface":{"columns":["name","options","type"]},
                "Bridge":{"columns":["controller","fail_mode","name","ports"]},
                "Controller":{"columns":[]},
                "Open_vSwitch":{"columns":["bridges","cur_cfg"]}}]}
 * *)
(*
 * {"method":"transact",
    "id":1,
    "params":["Open_vSwitch",
            {"rows":[{"interfaces":["uuid","bc7a3f28-2434-4935-a594-1d455f689f4f"]}],
             "columns":["interfaces"],
             "table":"Port",
             "until":"==",
             "where":[["_uuid","==",["uuid","fa9c0dc8-5a46-4770-8671-6379b4447350"]]],
             "timeout":0,"op":"wait"},
    {"rows":[{"interfaces":["uuid","d4e1009d-8571-4efd-ae0b-092adb1702ae"]}],
    "columns":["interfaces"],
    "table":"Port",
    "until":"==","where":[["_uuid","==",["uuid","04838f3d-b881-4ad0-bf20-9cb83368b437"]]],
    "timeout":0,"op":"wait"},
    {"rows":[{"bridges":["uuid","68339a8e-2989-494a-a79a-a54ae5d2c88d"]}],
    "columns":["bridges"],
    "table":"Open_vSwitch",
    "until":"==","where":[["_uuid","==",["uuid","ef7de61d-578a-479e-a755-e8fb3ffc5aab"]]],
    "timeout":0,"op":"wait"},
    {"rows":[{"interfaces":["uuid","4ac6602d-6d6f-45b5-a9c7-93979c840b59"]}],
    "columns":["interfaces"],"table":"Port","until":"==",
    "where":[["_uuid","==",["uuid","fde36b16-86b7-43ff-9d08-0ca5b03e5a7a"]]],
    "timeout":0,
    "op":"wait"},
    {"rows":[{"ports":["set",[["uuid","04838f3d-b881-4ad0-bf20-9cb83368b437"],
            ["uuid","fa9c0dc8-5a46-4770-8671-6379b4447350"],
            ["uuid","fde36b16-86b7-43ff-9d08-0ca5b03e5a7a"]]]}],
    "columns":["ports"],
    "table":"Bridge",
    "until":"==","where":[["_uuid","==",["uuid","68339a8e-2989-494a-a79a-a54ae5d2c88d"]]],
    "timeout":0,
    "op":"wait"},
    {"row":{"name":"gre0","interfaces":["named-uuid","row8d4c4e8b_6c25_4357_af87_ed6878927600"]},
    "table":"Port",
    "uuid-name":"row8956e4d1_4a4d_4fda_80b9_646e0e659a9f",
    "op":"insert"},
    {"row":{"ports":["set",[["uuid","04838f3d-b881-4ad0-bf20-9cb83368b437"],
    ["named-uuid","row8956e4d1_4a4d_4fda_80b9_646e0e659a9f"],
    ["uuid","fa9c0dc8-5a46-4770-8671-6379b4447350"],
    ["uuid","fde36b16-86b7-43ff-9d08-0ca5b03e5a7a"]]]},
    "table":"Bridge","where":[["_uuid","==",["uuid","68339a8e-2989-494a-a79a-a54ae5d2c88d"]]],
    "op":"update"},
    {"row":{"name":"gre0","options":["map",[["key","25"],["remote-ip","10.100.0.1"]]],"type":"gre"},
    "table":"Interface","uuid-name":"row8d4c4e8b_6c25_4357_af87_ed6878927600","op":"insert"},
    {"mutations":[["next_cfg","+=",1]],"table":"Open_vSwitch","where":[["_uuid","==",["uuid","ef7de61d-578a-479e-a755-e8fb3ffc5aab"]]],"op":"mutate"},
    {"columns":["next_cfg"],"table":"Open_vSwitch","where":[["_uuid","==",["uuid","ef7de61d-578a-479e-a755-e8fb3ffc5aab"]]],"op":"select"},
    {"comment":"ovs-vsctl: ovs-vsctl --db=tcp:127.0.0.1:6634 add-port xapi1 gre0 -- set interface gre0 type=gre options:remote-ip=10.100.0.1 options:key=25","op":"comment"}]
 *)

let main () =
    try_lwt 
        lwt c = create_controller () in
        lwt _ = add_app_of13 c 1l (Ipaddr.V4.of_string_exn "10.20.0.1") 
                (Macaddr.of_string_exn "fe:ff:ff:ff:ff:ff") 1  (Ipaddr.V4.of_string_exn "10.20.0.1") in 
        wait c
    with ex -> 
        return (Printf.printf "[ERR] %s\n%!" (Printexc.to_string ex))

let () =
    Lwt_main.run( main ())

