open Lwt

open OpenFlow0x04 
open OpenFlow0x04.Message
open OpenFlow0x04_Core

exception Controller_Error

type t ={
    of_istream: Lwt_io.input_channel;
    of_ostream: Lwt_io.output_channel;
    wait : unit Lwt.t;
    wake : unit Lwt.u;
    of_resp : (int32, Message.t Lwt.u) Hashtbl.t;
    xid_uid : int32 ref;
    mutable active: bool;
}

let wait t = t.wait

(*
 * Util functions
 * *)
let int64_of_macaddr mac =
    let iMac = ref 0L in
    let _ = String.iter (fun x -> 
        iMac := Int64.add (Int64.shift_left !iMac 8) (Int64.of_int (int_of_char x))) 
        (Macaddr.to_bytes mac) in
    !iMac

let send_of_msg c ?xid:(xid=(-1l)) m =
    let _ = 
        if not c.active then
            raise Controller_Error
    in
    let xid =
        if (xid < 0l) then 
            let _ = c.xid_uid := (Int32.succ !(c.xid_uid)) in
            !(c.xid_uid)
        else 
            xid
    in
(*    let _ = Printf.printf "< %s\n%!" (Message.to_string m) in *)
    let buf = Message.marshal xid m in
    Lwt_io.write_from_string_exactly c.of_ostream 
                buf 0 (String.length buf) 

let send_of_msg_reply c m = 
    let xid = !(c.xid_uid) in 
    let () = c.xid_uid := Int32.succ !(c.xid_uid) in
    let (t, u) = Lwt.wait () in 
    let _ = Hashtbl.add c.of_resp xid u in
    lwt () = send_of_msg c ~xid m in
    lwt r = t in
    let _ = Hashtbl.remove c.of_resp xid in
    return r

let send_of_msg_reply_timeout c m delay = 
    lwt r = (send_of_msg c m) <?> (lwt _ = Lwt_unix.sleep delay in fail_with "msg timeout") in
    return r


let of_proccess_msg c xid = function
    | Hello t -> 
(*            Printf.printf "> (%lu) Hello\n%!" xid; *)
            let u = Hashtbl.find c.of_resp 10l in
            let _ = Lwt.wakeup u (Hello t) in
            let _ = Hashtbl.remove c.of_resp 10l in
            send_of_msg c ~xid FeaturesRequest
    | EchoRequest _ -> 
(*            Printf.printf "> (%lu) EchoRequest\n%!" xid; *)
            send_of_msg c ~xid (EchoReply (Cstruct.create 0))
    | m ->
            let _ = 
                if (Hashtbl.mem c.of_resp xid) then
                    let _ = Lwt.wakeup (Hashtbl.find c.of_resp xid) m in 
                    Hashtbl.remove c.of_resp xid
                else ()
            in
            return ((* Printf.printf "> %s\n%!" (Message.to_string m) *)) 

let of_loop c process_msg =
    let buf = Bytes.create 2048 in 
    let rec loop_inner () =
        lwt _ = Lwt_io.read_into_exactly c.of_istream buf 0 (OpenFlow_Header.size) in
        let h = OpenFlow_Header.parse 
        (Cstruct.of_string 
            (Bytes. to_string (Bytes.sub buf 
            0 (OpenFlow_Header.size)))) in
        lwt (xid, m) = 
            match (h.OpenFlow_Header.length - OpenFlow_Header.size) with
            | 0 -> return (OpenFlow0x04.Message.parse h (String.make 0 '\000'))
            | len ->
                    lwt _ = Lwt_io.read_into_exactly c.of_istream buf 0 len in
                    let msg = Bytes. to_string (Bytes.sub buf 0 len) in
                    return (OpenFlow0x04.Message.parse h msg)
        in
        lwt _ = process_msg c xid m in 
            loop_inner ()
    in
    lwt _ = 
        try_lwt 
            loop_inner () 
        with e ->
            let _ = Printf.printf "Conection terminated with error: %s\n%!" (Printexc.to_string e) in 
            return (c.active <- false)
        in 
(*    return (Lwt.wakeup c.wake ()) *)
    return ()

let keepalive_check c = 
    while_lwt c.active do
        try_lwt 
            lwt _ = send_of_msg_reply_timeout c (EchoRequest (Cstruct.create 0)) 1.0 in
            Lwt_unix.sleep 1.0
        with _ -> 
            return (c.active <- false)
    done

let init_controller ?of_port:(of_port=6634) host = 
    let (wait, wake) = Lwt.wait () in 
    (* open the control channel *)
    let sockaddr = Unix.ADDR_INET((Unix.inet_addr_of_string (Ipaddr.V4.to_string host)), of_port) in
    lwt (of_istream, of_ostream) = Lwt_io.open_connection sockaddr in 
    let (t,u) = Lwt.wait () in
    let of_resp = Hashtbl.create 64 in
    let _ = Hashtbl.add of_resp 10l u in
    let c = {of_istream; of_ostream; wait; wake; xid_uid=(ref 0l); of_resp; active=true;} in
    lwt _ = send_of_msg c ~xid:10l (Hello []) in
    let _ = Lwt.ignore_result (of_loop c of_proccess_msg) in 
    
    (* wait for the SwitchFeature response, before any interaction *)
    lwt _ = t in

    (* Start thread to check echo responses *)
    let _ = Lwt.ignore_result (keepalive_check c) in 


    return c

let send_flow_mod c mfPriority mfOfp_match mfInstructions = 
    send_of_msg c (FlowModMsg  
        {mfCookie={m_value=0L; m_mask=None}; 
        mfTable_id=0;
        mfCommand=AddFlow;
        mfIdle_timeout=(ExpiresAfter 86400);
        mfHard_timeout=Permanent;mfPriority;
        mfBuffer_id=None;
        mfOut_port=None;
        mfOut_group=None;
        mfFlags={fmf_send_flow_rem=false;fmf_check_overlap=false;
            fmf_reset_counts=false;fmf_no_pkt_counts=false;fmf_no_byt_counts=false;};
        mfOfp_match;
        mfInstructions;}) 

let send_flow_mod_del c mfOfp_match = 
    send_of_msg c (FlowModMsg  
        {mfCookie={m_value=0L; m_mask=None}; 
        mfTable_id=0;
        mfCommand=DeleteFlow;
        mfIdle_timeout=Permanent;
        mfHard_timeout=Permanent;
        mfPriority=0;
        mfBuffer_id=None;
        mfOut_port=None;
        mfOut_group=None;
        mfFlags={fmf_send_flow_rem=false;fmf_check_overlap=false;
            fmf_reset_counts=false;fmf_no_pkt_counts=false;fmf_no_byt_counts=false;};
        mfOfp_match;
        mfInstructions=[];}) 


let get_port c vif_name = 
    try_lwt 
        let vif_id = String.make 16 '\000' in
        let () = String.blit vif_name 0 vif_id 0 (String.length vif_name) in
    
        lwt ports = 
            match_lwt (send_of_msg_reply c (MultipartReq ({mpr_type=PortsDescReq;mpr_flags=false;}))) with
            | MultipartReply t -> begin 
                    match t.mpreply_typ with
                    | PortsDescReply ports -> return ports
                    | _ -> return []
                    end
            | _ -> return []
        in
        return (List.find (
            fun (x : OpenFlow0x04_Core.portDesc) -> 
                Printf.printf "%s %s\n%!" x.name vif_id;
                x.name = vif_id) 
        ports )
    with Not_found -> 
        failwith "get port not found!"

let get_group_stats c appid = 
    match_lwt (send_of_msg_reply c (MultipartReq ({mpr_type=(GroupStatsReq appid);mpr_flags=false;}))) with 
    | MultipartReply p -> begin
        match p.mpreply_typ with
        | GroupStatsReply [] -> return None
        | GroupStatsReply r -> return (Some r)
        | _ -> return None
    end 
    | _ -> return None

let get_group_buckets c appid = 
    match_lwt (send_of_msg_reply c (MultipartReq ({mpr_type=GroupDescReq;mpr_flags=false;}))) with
    | MultipartReply r -> begin 
        match r.mpreply_typ with
        | GroupDescReply groups -> 
            let groupDesc = List.find (fun (x:OpenFlow0x04_Core.groupDesc) -> x.group_id = appid) groups  in
            return (groupDesc.bucket)
        | _ -> failwith "Group not found"
    end
    | _ -> failwith "Group not found"



let add_vm c ?ext_port:(ext_port=1l) ip mac port = 
    (* Add the default forwarding rules *)
    lwt _ = send_flow_mod c 2
                [(OxmInPort port.OpenFlow0x04_Core.port_no);]
                [(ApplyActions [(Output (PhysicalPort ext_port))])]
    in

    lwt _ = send_flow_mod c 3
                [(OxmEthType 0x0806);(OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;});]
                [(ApplyActions [(Output (PhysicalPort port.port_no))])]
    in
    lwt _ = send_flow_mod c 3
                [(OxmEthType 0x0800);(OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
                [(ApplyActions [(Output (PhysicalPort port.port_no))])]
    in
    lwt _ = send_flow_mod c 3
                [(OxmEthType 0x0806);(OxmARPTpa {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
                [(ApplyActions [(Output (PhysicalPort port.port_no))])]
    in
    return ()

let extract_service_ip buckets =
    let rec extract_service_ip_inner sip smac = function
        | [] -> (sip, smac)
        | SetField ( OxmIP4Dst x )::t ->
                extract_service_ip_inner x.m_value smac t
        | SetField ( OxmEthDst x ) :: t ->
                extract_service_ip_inner sip x.m_value t
        | _::t -> 
                extract_service_ip_inner sip smac t
    in 
    extract_service_ip_inner 0l 0L (List.hd buckets).bu_actions 

let init_of_group c appid ip mac port = 
    match_lwt get_group_stats c appid with 
    | None 
    | Some [] -> 
        send_of_msg c (GroupModMsg (AddGroup (Select, appid, 
            [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
            bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                (Output (PhysicalPort port))]
            }]))) 
    | Some _ ->
        send_of_msg c (GroupModMsg (ModifyGroup (Select, appid, 
            [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
            bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
            (Output (PhysicalPort port))]}]))) 

let append_of_group_bucket c appid _ _ bu_actions =
    lwt buckets = get_group_buckets c appid in
    send_of_msg c (GroupModMsg (ModifyGroup (Select, appid, 
           buckets @
           [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
           bu_actions;}])))
 
let disable_of_group_bucket c appid port =
    lwt buckets = get_group_buckets c appid in
    Printf.printf "Disabling output port %ld\n%!" port;
    let new_buckets = 
        List.map
            (fun a -> 
                let rec is_port port = function
                    | Output (PhysicalPort p)::_ when (p = port) -> true
                    | _::t -> is_port port t
                    | [] -> false
                in
                if (is_port port a.bu_actions) then 
                    {a with bu_weight=0;}
                else a) buckets in
    send_of_msg c (GroupModMsg (ModifyGroup (Select, appid, new_buckets)))

let delete_of_group_bucket c appid port =
    lwt buckets = get_group_buckets c appid in
    let new_buckets = 
        List.filter
            (fun a -> 
                let rec is_port port = function
                    | Output (PhysicalPort p)::_ when (p = port) -> false
                    | _::t -> is_port port t
                    | [] -> true
                in
                is_port port a.bu_actions) buckets in
    send_of_msg c (GroupModMsg (ModifyGroup (Select, appid, new_buckets)))
