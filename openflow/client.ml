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
let ovsdb_port = 6634
let intf_name_constant = "vif"
let ext_port = 1l

type t = {
    of_istream: Lwt_io.input_channel;
    of_ostream: Lwt_io.output_channel;
    db_fd : Lwt_unix.file_descr;
    wait : unit Lwt.t;
    wake : unit Lwt.u;
    host : Ipaddr.V4.t;
    of_resp : (int32, Message.t Lwt.u) Hashtbl.t;
    db_resp : (int32, Rpc.t Lwt.u) Hashtbl.t;
    xid_uid : int32 ref;
    dbid_uid : int32 ref;
}

(*
 * Util functions
 * *)
let int64_of_macaddr mac =
    let iMac = ref 0L in
    let _ = String.iter (fun x -> 
        iMac := Int64.add (Int64.shift_left !iMac 8) (Int64.of_int (int_of_char x))) 
        (Macaddr.to_bytes mac) in
    !iMac

let string_of_ipaddr ip = 
    let x = ref "" in
    let () = String.iter (fun a -> x := Printf.sprintf "%s%02x" !x (int_of_char a)) (Ipaddr.V4.to_bytes ip) in
    !x
(*
 * RPC manipulation operations
 * *)
let rec find_element name = function
    | [] -> failwith "dict entry not found"
    | (n, t) :: _ when n = name -> t
    | _ :: r -> find_element name r
let rec exists_element name = function
    | [] -> false
    | (n, t) :: _ when n = name -> true
    | _ :: r -> find_element name r


let dict_of_rpc = function
    | (Rpc.Dict a) -> a
    | _ -> failwith "Not RPC Dict"

let enum_of_rpc = function
    | Rpc.Enum a -> a
    | _ -> failwith "Not RPC Enum"


(*
 * OpenFlow background control manipulation 
 * *)
let send_of_msg c ?xid:(xid=(-1l)) m =
    let xid =
        if (xid < 0l) then 
            let _ = c.xid_uid := Int32.succ !(c.xid_uid) in
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
        lwt _ = read_into_exactly c.of_istream buf 0 (OpenFlow_Header.size) in
        let h = OpenFlow_Header.parse 
        (Cstruct.of_string 
            (Bytes. to_string (Bytes.sub buf 
            0 (OpenFlow_Header.size)))) in
        lwt (xid, m) = 
            match (h.OpenFlow_Header.length - OpenFlow_Header.size) with
            | 0 -> return (OpenFlow0x04.Message.parse h (String.make 0 '\000'))
            | len ->
                    lwt _ = read_into_exactly c.of_istream buf 0 len in
                    let msg = Bytes. to_string (Bytes.sub buf 0 len) in
                    return (OpenFlow0x04.Message.parse h msg)
        in
        lwt _ = process_msg c xid m in 
            loop_inner ()
    in
    lwt _ = loop_inner () in 
    return (Lwt.wakeup c.wake ())

(*
 * OVSDB background manipulation operations
 * *)
let db_invoke_call c m p = 
    let id = !(c.dbid_uid) in 
    let () = c.dbid_uid := Int32.succ !(c.dbid_uid) in
    let buf = Jsonrpc.to_string Rpc.(Dict [
        ("method", (String m));
        ("params", p);
        ("id", (Int32 id))
    ]) in
    let () = Printf.printf "<< %s\n%!" buf in 
    let (t, u) = Lwt.wait () in 
    let _ = Hashtbl.add c.db_resp id u in
    lwt _ = Lwt_unix.write_string c.db_fd 
                buf 0 (String.length buf) in
    lwt r = t in
    let _ = Hashtbl.remove c.db_resp id in
    return r

let db_invoke_call_nowait c m p =  
    let id = !(c.dbid_uid) in 
    let () = c.dbid_uid := Int32.succ !(c.dbid_uid) in
    let buf = Jsonrpc.to_string Rpc.(Dict [
        ("method", (String m));
        ("params", p);
        ("id", (Int32 id))
    ]) in
    let () = Printf.printf "<< %s\n%!" buf in 
    lwt _ = Lwt_unix.write_string c.db_fd 
                buf 0 (String.length buf) in
    return ()

let db_process_call c xid call =
    match call.Rpc.name with
    | "echo" ->
            let buf = Jsonrpc.to_string 
            Rpc.(Dict  [("id", xid); ("result", (Enum [])); ("error", Null) ]) in
(*            let _ = Printf.printf ">> %s\n%!" buf in *)
            lwt _ = Lwt_unix.write_string c.db_fd buf 0 (String.length buf) in 
            return ()
    | _ -> return (Printf.printf "%% ERROR DB Unsupported %s\n%!" (Rpc.string_of_call call))

let db_loop c =
    let rec db_loop_inner () =
        let buf = Bytes.make 4096 '\000' in
        lwt len = Lwt_unix.read c.db_fd buf 0 4096 in
        let resp = Jsonrpc.of_string (Bytes.sub_string buf 0 len) in 
        let _ = Printf.printf ">> %s\n%!" (Jsonrpc.to_string resp) in 
        let data = dict_of_rpc resp in
        lwt () =
            if (List.exists (fun (n, t) -> n = "method" ) data ) then
                let name = Rpc.string_of_rpc (find_element "method" data) in
                let params = enum_of_rpc (find_element "params" data) in 
                let id = find_element "id" data in
                db_process_call c id (Rpc.({name; params;})) 
            else begin
                if (List.exists (fun (n, t) -> n = "result" ) data ) then
                    let id = Rpc.int32_of_rpc (find_element "id" data) in
                    let _ = 
                        if (Hashtbl.mem c.db_resp id) then
                            Lwt.wakeup (Hashtbl.find c.db_resp id) (Rpc.Dict data)
                    in
                        return ()
                else 
                    return ((*Printf.printf "ERROR DB unsupported %s\n%!" (Jsonrpc.to_string resp) *))
            end
        in
        db_loop_inner ()
    in
    try_lwt 
        db_loop_inner ()
    with e ->
        return (Printf.printf "ERR DB %s\n%!" (Printexc.to_string e))

let wait c = c.wait

let create_controller host = 
    let (wait, wake) = Lwt.wait () in 
    
    (* open the of ovsdb channel *)
    let db_fd = Lwt_unix.of_unix_file_descr (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0) in
    let sockaddr = Unix.ADDR_INET((Unix.inet_addr_of_string (Ipaddr.V4.to_string host)), ovsdb_port) in
    let () = Lwt_unix.setsockopt db_fd Unix.TCP_NODELAY true in
    lwt () = Lwt_unix.connect db_fd sockaddr in

    (* open the control channel *)
    let sockaddr = Unix.ADDR_INET((Unix.inet_addr_of_string (Ipaddr.V4.to_string host)), of_port) in
    lwt (of_istream, of_ostream) = Lwt_io.open_connection sockaddr in 
    let c = {of_istream; of_ostream; wait; db_fd; wake; of_resp=(Hashtbl.create 64); 
                 db_resp=(Hashtbl.create 64); xid_uid=(ref 100l);dbid_uid=(ref 100l);host;} in
    let (t,u) = Lwt.wait () in
    let _ = Hashtbl.add c.of_resp 10l u in
    lwt _ = send_of_msg c ~xid:10l (Hello []) in

    (* run daemons on the background *)
    let _ = Lwt.ignore_result (of_loop c of_proccess_msg) in 
    let _ = Lwt.ignore_result (db_loop c) in  

    (* wait for the SwitchFeature response, before any interaction *)
    lwt _ = t in
    return c


let db_add_tunnel c br endpoint name domid = 
    (* First get details from the ovsdb *)
    lwt resp =  db_invoke_call c "monitor"
            Rpc.(Enum [
                (String "Open_vSwitch"); Null;
                (Dict ["Bridge", (Dict ["columns", (Enum [(String "name"); (String "ports")])]);
                ("Port", (Dict ["columns", (Enum [(String "name"); (String "interfaces")])]));
                 ("Open_vSwitch", (Dict ["columns", (Enum [(String "cur_cfg")])]));]);
            ]) in 
    let bridges = dict_of_rpc 
    (find_element "Bridge" 
    (dict_of_rpc (find_element "result" (dict_of_rpc resp)))) in
    let (uuid, br, interfaces) = 
        List.find (fun (uuid, name, interfaces) -> br = name) 
        (List.map (fun (uuid, t) -> 
            let name = Rpc.string_of_rpc (find_element "name"(
                    dict_of_rpc (find_element "new" (dict_of_rpc t)))) in
            let interfaces = enum_of_rpc (List.nth (enum_of_rpc (
                find_element "ports" (dict_of_rpc (find_element "new"(dict_of_rpc t)))
                )) 1) in
            (uuid, name, interfaces)
        ) bridges) in
    let (ovs_uuid, _) :: [] = dict_of_rpc (find_element "Open_vSwitch" (dict_of_rpc (find_element "result" (dict_of_rpc resp))))  in

    (* Send a big transaction to set up the new tunnel across the databse tables *)
    lwt () =  db_invoke_call_nowait c "transact"
            Rpc.(Enum [
                (String "Open_vSwitch");
                (Dict [("op", (String "insert")); 
                       ("table", (String "Interface"));
                       ("row", (Dict [
                           ("type", (String "gre")); 
                           ("name", (String name));
                           ("options", (Enum [
                               (String "map");
                               (Enum [ (Enum [(String "remote_ip");(String (Ipaddr.V4.to_string endpoint))]); (Enum [(String "key");(String (string_of_int domid ) )])])
                               ]))
                       ]));
                       ("uuid-name",(String "new_iface"));
                       ]);
                (Dict [
                    ("op", (String "insert")); 
                    ("table", (String "Port")); 
                    ("uuid-name", (String "new_port")); 
                    ("row", (Dict [
                        ("name", (String name)); 
                        ("interfaces", (Enum [(String "named-uuid");(String "new_iface")]))
                    ]));
                ]);
                (Dict [
                    ("op", (String "update")); 
                    ("table", (String "Bridge"));
                    ("where", (Enum [
                        (Enum [ 
                            (String "_uuid"); (String "=="); 
                            (Enum [(String "uuid"); (String uuid)])
                        ])
                    ]));
                    ("row", 
                        (Dict[("ports", (Enum [(String "set"); (Enum ((Enum [(String "named-uuid"); (String "new_port")]) :: interfaces) );]))]))
                ]);
                (Dict [
                    ("table", (String "Open_vSwitch"));
                    ("op", (String "mutate"));
                    ("mutations", (Enum [(Enum [(String "next_cfg"); (String "+="); (Int32 1l)])]));
                    ("where", (Enum [
                        (Enum [ 
                            (String "_uuid"); (String "=="); 
                            (Enum [(String "uuid"); (String ovs_uuid)])
                        ])
                    ]));
                ]);
                (Dict [("op", (String "comment")); ("comment", (String "jitsu adds new port"))])
            ])
    in
    return ()

let db_del_tunnel c br endpoint name domid = 
    (* First get details from the ovsdb *)
    lwt resp =  db_invoke_call c "monitor"
            Rpc.(Enum [
                (String "Open_vSwitch"); Null;
                (Dict ["Bridge", (Dict ["columns", (Enum [(String "name"); (String "ports")])]);
                ("Port", (Dict ["columns", (Enum [(String "name"); (String "interfaces")])]));
                ("Open_vSwitch", (Dict ["columns", (Enum [(String "cur_cfg")])]));]);
            ]) in 
    let bridges = dict_of_rpc 
    (find_element "Bridge" 
    (dict_of_rpc (find_element "result" (dict_of_rpc resp)))) in
    let (uuid, br, interfaces) = 
        List.find (fun (uuid, name, interfaces) -> br = name) 
        (List.map (fun (uuid, t) -> 
            let name = Rpc.string_of_rpc (find_element "name"(
                    dict_of_rpc (find_element "new" (dict_of_rpc t)))) in
            let interfaces = enum_of_rpc (List.nth (enum_of_rpc (
                find_element "ports" (dict_of_rpc (find_element "new"(dict_of_rpc t)))
                )) 1) in
            (uuid, name, interfaces)
        ) bridges) in

    let ports = dict_of_rpc 
                    (find_element "Port" 
                    (dict_of_rpc (find_element "result" (dict_of_rpc resp)))) in
    let (port_uuid, name) = 
        List.find (fun (uuid, n, interfaces) -> name = n) 
        (List.map (fun (uuid, t) -> 
            let name = find_element "name" (dict_of_rpc (find_element "new"(dict_of_rpc t))) in
            (uuid, name)
        ) ports) in
    let (ovs_uuid, _) :: [] = dict_of_rpc (find_element "Open_vSwitch" (dict_of_rpc (find_element "result" (dict_of_rpc resp))))  in

    (* Send a big transaction to set up the new tunnel across the databse tables *)
    lwt () =  db_invoke_call_nowait c "transact"
            Rpc.(Enum [
                (String "Open_vSwitch");
                (Dict [
                    ("op", (String "update")); 
                    ("table", (String "Bridge"));
                    ("where", (Enum [
                        (Enum [ 
                            (String "_uuid"); (String "=="); 
                            (Enum [(String "uuid"); (String uuid)])
                        ])
                    ]));
                    ("row", 
                        (Dict[("ports", (Enum [(String "set"); (Enum 
                            ((Enum [(String "named-uuid"); (String "new_port")]) :: interfaces) );
                        ]))]))
                ]);
                (Dict [
                    ("table", (String "Open_vSwitch"));
                    ("op", (String "mutate"));
                    ("mutations", (Enum [(Enum [(String "next_cfg"); (String "+="); (Int32 1l)])]));
                    ("where", (Enum [
                        (Enum [ 
                            (String "_uuid"); (String "=="); 
                            (Enum [(String "uuid"); (String ovs_uuid)])
                        ])
                    ]));
                ]);
                (Dict [("op", (String "comment")); ("comment", (String "jitsu adds new port"))])
            ])
    in
    return ()

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
    return (List.find (fun (x : PortDesc.t) -> Printf.printf "%s %s\n%!" x.name vif_id;x.name = vif_id) ports )
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
            let groupDesc = List.find (fun (x:groupDesc) -> x.group_id = appid) groups  in
            return (groupDesc.bucket)
        | _ -> failwith "Group not found"
    end
    | _ -> failwith "Group not found"

let add_vm c ip mac port = 
    (* Add the default forwarding rules *)
    lwt _ = send_flow_mod c 2
                [(OxmInPort port.port_no);]
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

let add_app_of13 c appid ip mac domid sip =
    (* First get the port num for the specific domid *)
    let vif_name = Printf.sprintf "%s%d.0" intf_name_constant domid in
    lwt port = get_port c vif_name in
    lwt () = add_vm c ip mac port in

    (* Add the new group entry *)
    lwt () = 
        match_lwt get_group_stats c appid with 
        | None -> 
            send_of_msg c (GroupModMsg (AddGroup (Select, appid, 
                [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
                bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                    (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                    (Output (PhysicalPort port.port_no))]
                }]))) 
        | Some _ ->
            send_of_msg c (GroupModMsg (ModifyGroup (Select, appid, 
                [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
                bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                    (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                (Output (PhysicalPort port.port_no))]}]))) 
    in 

    (* Add the forwarding rule for the service port number *)
    lwt _ = send_flow_mod c 10
                [(OxmEthType 0x0800);(OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 sip);m_mask=None;});]
                [(ApplyActions [(Group appid)])]
    in

    (* Add the inverse path *)
    lwt _ = send_flow_mod c 10 [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
                (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
                [(ApplyActions [(SetField (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 sip);m_mask=None;})); 
                (Output (PhysicalPort ext_port))])] 
    in

    return ()

let add_replica_of13 src_c dst_c appid ip mac domid =
    (* First get the port num for the specific domid *)
    let vif_name = Printf.sprintf "%s%d.0" intf_name_constant domid in
    lwt port = get_port dst_c vif_name in
    
    (* get the original replica IP and MAC address *)
    lwt () = add_vm dst_c ip mac port in
    lwt buckets = get_group_buckets src_c appid in

    let (sip, smac) = extract_service_ip buckets in

    lwt _ = 
        if (src_c == dst_c) then
            send_of_msg src_c (GroupModMsg (ModifyGroup (Select, appid, 
                buckets @
                [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
                bu_actions=[(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                    (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                (Output (PhysicalPort port.port_no))]}])))
            else 
                let name = Printf.sprintf "gre%s%02x" (string_of_ipaddr dst_c.host) domid in
                lwt _ = db_add_tunnel src_c "xenbr2" dst_c.host name domid in
                lwt _ = db_add_tunnel dst_c "xenbr2" src_c.host name domid in
                lwt () = Lwt_unix.sleep 1.0 in
                lwt local_port = get_port src_c name in
                lwt remote_port = get_port dst_c name in

                lwt () = send_flow_mod dst_c 10 
                        [(OxmInPort remote_port.port_no);
                         (OxmEthType 0x0800);
                         (OxmIP4Dst {m_value=sip;m_mask=None;});]
                        [(ApplyActions [(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                         (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                         (Output (PhysicalPort port.port_no))])] in

               lwt () = send_of_msg src_c (GroupModMsg (ModifyGroup (Select, appid, 
                        buckets @
                        [{bu_weight=50; bu_watch_port=None; bu_watch_group=None;
                        bu_actions=[(Output (PhysicalPort local_port.port_no))]}]))) in 
                lwt () = send_flow_mod dst_c 9 [(OxmInPort remote_port.port_no);] [] in
                lwt () = send_flow_mod src_c 9 [(OxmInPort local_port.port_no);] [] in

                return ()
    in

    (* Add the inverse path *)
    lwt _ = send_flow_mod dst_c 10 [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
        (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
        [(ApplyActions [(SetField (OxmIP4Src {m_value=sip;m_mask=None;})); 
        (Output (PhysicalPort ext_port))])] in
    return ()

let disable_replica_of13 src_c dst_c appid ip domid =
    lwt buckets = get_group_buckets src_c appid in
    let (sip, smac) = extract_service_ip buckets in
    lwt vif_name = 
        if (src_c == dst_c) then
            return (Printf.sprintf "%s%d.0" intf_name_constant domid)
        else 
            let vif_name = Printf.sprintf "gre%s%02x" (string_of_ipaddr dst_c.host) domid in
                lwt port = get_port dst_c vif_name in
                lwt () = send_flow_mod_del dst_c
                        [(OxmInPort port.port_no);
                         (OxmEthType 0x0800);
                         (OxmIP4Dst {m_value=sip;m_mask=None;});] in
                return (vif_name)
    in
    lwt port = get_port src_c vif_name in
    let new_buckets = 
        List.map
            (fun a -> 
                let rec is_port port = function
                    | Output (PhysicalPort p)::_ when (p = port.port_no) -> true
                    | _::t -> is_port port t
                    | [] -> false
                in
                if (is_port port a.bu_actions) then 
                    {a with bu_weight=0;}
                else a) buckets in
    lwt _ = send_of_msg src_c (GroupModMsg (ModifyGroup (Select, appid, new_buckets))) in
    lwt _ = send_flow_mod_del dst_c [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
        (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});] in

    return ()

let delete_replica_of13 src_c dst_c appid ip domid =
    lwt buckets = get_group_buckets src_c appid in
    lwt vif_name = 
        if (src_c == dst_c) then
            return (Printf.sprintf "%s%d.0" intf_name_constant domid)
        else 
            return (Printf.sprintf "gre%s%02x" (string_of_ipaddr dst_c.host) domid)
    in
    lwt port = get_port src_c vif_name in

    let new_buckets = 
        List.fold_right
            (fun a b -> 
                let rec is_port port = function
                    | Output (PhysicalPort p)::_ when (p = port.port_no) -> true
                    | _::t -> is_port port t
                    | [] -> false
                in
                if (is_port port a.bu_actions) then 
                    b
                else b@[a]) buckets [] in
    lwt _ = send_of_msg src_c (GroupModMsg (ModifyGroup (Select, appid, new_buckets))) in
    (* remove the inverse path *)
    lwt _ = send_flow_mod_del dst_c [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
        (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});] in
    return ()


let main () =
    try_lwt 
        lwt src_c = create_controller (Ipaddr.V4.of_string_exn "10.20.0.9") in
        lwt dst_c = create_controller (Ipaddr.V4.of_string_exn "10.20.0.101") in
        lwt _ = add_app_of13 src_c 1l (Ipaddr.V4.of_string_exn "10.20.0.94") 
                (Macaddr.of_string_exn "1e:77:f0:b4:bf:23") 1  
                (Ipaddr.V4.of_string_exn "10.20.0.94") in 
        lwt _ = add_replica_of13 src_c src_c 1l (Ipaddr.V4.of_string_exn "10.20.0.97") 
                (Macaddr.of_string_exn "ba:bc:41:78:5a:ab") 2 in
        lwt _ = add_replica_of13 src_c dst_c 1l (Ipaddr.V4.of_string_exn "10.20.0.99") 
                (Macaddr.of_string_exn "1e:f8:92:18:6b:06") 65 in
        lwt _ = disable_replica_of13 src_c src_c 1l (Ipaddr.V4.of_string_exn "10.20.0.97") 2 in
        lwt _ = disable_replica_of13 src_c dst_c 1l (Ipaddr.V4.of_string_exn "10.20.0.99") 65 in
        wait src_c
    with ex -> 
        return (Printf.printf "[ERR] %s\n%!" (Printexc.to_string ex))

let () =
    Lwt_main.run( main ())
