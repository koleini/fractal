open Lwt
open Of13controller
open DbController

let of_port = 6633
let ovsdb_port = 6634
let intf_name_constant = "vif"
let ext_port = 1l

type t = {
    ofc: Of13controller.t;
    dbc: DbController.t;
    host : Ipaddr.V4.t;
}

(*
 * Util methods
 * *)
let string_of_ipaddr ip = 
    let x = ref "" in
    let () = String.iter (fun a -> x := Printf.sprintf "%s%02x" !x (int_of_char a)) (Ipaddr.V4.to_bytes ip) in
    !x

let add_app_of13 c appid ip mac domid sip =
    let open OpenFlow0x04_Core in

    (* First get the port num for the specific domid *)
    let vif_name = Printf.sprintf "%s%d.0" intf_name_constant domid in
    lwt port = get_port c.ofc vif_name in
    lwt () = add_vm c.ofc ip mac port in

    (* Add the new group entry *)
    lwt _ = init_of_group c.ofc appid ip mac port.port_no in

    (* Add the forwarding rule for the service port number *)
    lwt _ = send_flow_mod c.ofc 10
                [(OxmEthType 0x0800);(OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 sip);m_mask=None;});]
                [(ApplyActions [(Group appid)])]
    in

    (* Add the inverse path *)
    lwt _ = send_flow_mod c.ofc 10 [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
                (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
                [(ApplyActions [(SetField (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 sip);m_mask=None;})); 
                (Output (PhysicalPort ext_port))])] 
    in

    return ()

let add_replica_of13 src_c dst_c appid ip mac domid =
    let open OpenFlow0x04_Core in
    
    (* First get the port num for the specific domid *)
    let vif_name = Printf.sprintf "%s%d.0" intf_name_constant domid in
    lwt port = get_port dst_c.ofc vif_name in
    
    (* get the original replica IP and MAC address *)
    lwt () = add_vm dst_c.ofc ip mac port in
    lwt buckets = get_group_buckets src_c.ofc appid in

    let (sip, _) = extract_service_ip buckets in

    lwt _ = 
        if (src_c == dst_c) then
            append_of_group_bucket src_c.ofc appid ip mac
                [(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                (Output (PhysicalPort port.port_no))]
        else 
           let name = Printf.sprintf "gre%s%02x" (string_of_ipaddr dst_c.host) domid in
           lwt _ = db_add_tunnel src_c.dbc "xenbr2" dst_c.host name domid in
           lwt _ = db_add_tunnel dst_c.dbc "xenbr2" src_c.host name domid in
           lwt local_port = get_port src_c.ofc name in
           lwt remote_port = get_port dst_c.ofc name in

           lwt () = send_flow_mod dst_c.ofc 10 
                   [(OxmInPort remote_port.port_no);
                    (OxmEthType 0x0800);
                    (OxmIP4Dst {m_value=sip;m_mask=None;});]
                   [(ApplyActions [(SetField (OxmIP4Dst {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;}));
                    (SetField (OxmEthDst {m_value=(int64_of_macaddr mac);m_mask=None;})); 
                    (Output (PhysicalPort port.port_no))])] in
            lwt () = append_of_group_bucket src_c.ofc appid ip mac [(Output (PhysicalPort local_port.port_no))] in
            lwt () = send_flow_mod dst_c.ofc 9 [(OxmInPort remote_port.port_no);] [] in
            lwt () = send_flow_mod src_c.ofc 9 [(OxmInPort local_port.port_no);] [] in
           return ()
    in

    (* Add the inverse path *)
    lwt _ = send_flow_mod dst_c.ofc 10 [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
        (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});]
        [(ApplyActions [(SetField (OxmIP4Src {m_value=sip;m_mask=None;})); 
        (Output (PhysicalPort ext_port))])] in
    return ()

let disable_replica_of13 src_c dst_c appid _ domid =
    let open OpenFlow0x04_Core in
    lwt vif_name = 
        if (src_c == dst_c) then
            return (Printf.sprintf "%s%d.0" intf_name_constant domid)
        else 
            let vif_name = Printf.sprintf "gre%s%02x" (string_of_ipaddr dst_c.host) domid in

                return (vif_name)
    in
    lwt port = get_port src_c.ofc vif_name in
    lwt () = disable_of_group_bucket src_c.ofc appid port.port_no in
    Printf.printf "disabling stuff \n%!";
    return ()

let delete_replica_of13 src_c dst_c appid ip domid =
    let open OpenFlow0x04_Core in
    lwt buckets = get_group_buckets src_c.ofc appid in
    let (sip, _) = extract_service_ip buckets in
    Printf.printf "deleting stuff \n%!";
    lwt port = 
        if (src_c == dst_c) then
            get_port src_c.ofc  (Printf.sprintf "%s%d.0" intf_name_constant domid)
        else 
            let vif_name = Printf.sprintf "gre%s%02x" (string_of_ipaddr dst_c.host) domid in
            lwt port = get_port dst_c.ofc vif_name in
            lwt () = send_flow_mod_del dst_c.ofc
                        [(OxmInPort port.port_no);
                         (OxmEthType 0x0800);
                         (OxmIP4Dst {m_value=sip;m_mask=None;});] in
            lwt _ = db_del_tunnel dst_c.dbc "xenbr2" dst_c.host vif_name domid in
            lwt port = get_port src_c.ofc vif_name in
            lwt _ = db_del_tunnel src_c.dbc "xenbr2" src_c.host vif_name domid in
            return port
    in
    lwt () = delete_of_group_bucket src_c.ofc appid port.port_no in

    (* remove the inverse path *)
    lwt _ = send_flow_mod_del dst_c.ofc [(OxmEthType 0x0800);(OxmIPProto 6);(OxmTCPSrc 80);
        (OxmIP4Src {m_value=(Ipaddr.V4.to_int32 ip);m_mask=None;});] in
    return ()

let wait c = Of13controller.wait c.ofc


let create_controller host = 
   (* Open the control channel *)
    lwt ofc = Of13controller.init_controller ~of_port host in 

    lwt dbc = DbController.init_controller ~ovsdb_port host in 
    (* open the of ovsdb channel *)
    let c = {ofc; dbc; host;} in

    return c



