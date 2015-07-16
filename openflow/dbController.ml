open Lwt

type json = 
    [ `Bool of bool 
    | `Null 
    | `Float of float
    | `String of string
    | `A of json list 
    | `O of (string * json) list ]
exception Escape of ((int * int) * (int * int)) * Jsonm.error

type t = {
    db_fd : Lwt_unix.file_descr;
    db_resp : (int32, json Lwt.u) Hashtbl.t;
    dbid_uid : int32 ref;
    (* intf_name -> (uuid, port_uuid)*)
    db_port : (string, (string * string)) Hashtbl.t;
    (* bridge_name -> (uuid, [intf_uuid])*)
    db_br : (string, (string * string list)) Hashtbl.t;
    mutable ovs_uuid : string;
}

(*
 * JSON RPC utils
 * *)
let json_of_src ?encoding src =
  let dec d = match Jsonm.decode d with 
  | `Lexeme l -> l
  | `Error e -> raise (Escape (Jsonm.decoded_range d, e))
  | `End | `Await -> assert false
  in
  let rec value v k d = match v with 
  | `Os -> obj [] k d  | `As -> arr [] k d
  | `Null | `Bool _ | `String _ | `Float _ as v -> k v d 
  | _ -> assert false
  and arr vs k d = match dec d with 
  | `Ae -> k (`A (List.rev vs)) d
  | v -> value v (fun v -> arr (v :: vs) k) d
  and obj ms k d = match dec d with 
  | `Oe -> k (`O (List.rev ms)) d
  | `Name n -> value (dec d) (fun v -> obj ((n, v) :: ms) k) d
  | _ -> assert false
  in
  let d = Jsonm.decoder ?encoding src in
  try 
      let ret = `JSON (value (dec d) (fun v _ -> v) d) in
      let ((_,_),(_,len)) = Jsonm.decoded_range d in
      (len, ret)
  with 
  | Escape (r, e) -> (0, `Error (r, e))

let json_to_dst ~minify dst json =
  let enc e l = ignore (Jsonm.encode e (`Lexeme l)) in
  let rec value v k e = match v with 
  | `A vs -> arr vs k e 
  | `O ms -> obj ms k e 
  | `Null | `Bool _ | `Float _ | `String _ as v -> enc e v; k e
  and arr vs k e = enc e `As; arr_vs vs k e
  and arr_vs vs k e = match vs with 
  | v :: vs' -> value v (arr_vs vs' k) e 
  | [] -> enc e `Ae; k e
  and obj ms k e = enc e `Os; obj_ms ms k e
  and obj_ms ms k e = match ms with 
  | (n, v) :: ms -> enc e (`Name n); value v (obj_ms ms k) e
  | [] -> enc e `Oe; k e
  in
  let e = Jsonm.encoder ~minify (`Buffer dst) in
  let finish e = ignore (Jsonm.encode e `End) in
  match json with `A _ | `O _ as json -> value json finish e
  | _ -> invalid_arg "invalid json text"

let str = function
    | `String a -> a
    | _ -> failwith "str failed"
let intj = function
    | `Float f -> (Int32.of_float f)
    | _ -> failwith "int32_of_string failed"
let rec el name = function
    | [] -> failwith "dict entry not found"
    | (n, t) :: _ when n = name -> t
    | _ :: r -> el name r
let rec el_ex (name:string) = function
    | [] -> false
    | (n, _) :: _ when n = name -> true
    | _ :: r -> el_ex name r
let dict = function
    | (`O a) -> a
    | _ -> failwith "Not RPC Dict"
let enum = function
    | `A a -> a
    | _ -> failwith "Not RPC Enum"

let json_to_string m = 
    let buf = Buffer.create 4096 in
    let () = json_to_dst ~minify:true buf m in
    Buffer.contents buf
(*
 *
 * OVSDB background manipulation operations
 * *)
let db_update_state c resp = 
    (* First get details from the ovsdb *)
    let _ = 
        List.iter ( fun (m, a) -> 
            match m with
            | "Port" -> 
                    let ports = dict a in
                    List.iter (fun (uuid, t) ->
                        if (el_ex "new" (dict t)) then 
                        let name = str (el "name"( dict (el "new" (dict t)))) in
                        let intf = str (List.nth (enum (el "interfaces"( dict (el "new" (dict t))))) 1) in
                        let () = Printf.printf "port %s -> (%s, %s)\n%!" uuid name intf in 
                        Hashtbl.add c.db_port name (uuid, intf)  
                        else ()
                        ) ports
            | "Bridge" ->
                    let bridges = dict a in
                    (List.iter (fun (uuid, t) -> 
                        let name = str (el "name"( dict (el "new" (dict t)))) in
                        let interfaces = 
                            List.map (fun a -> 
                                str (List.nth (enum a) 1))
                            (enum (List.nth (enum (el "ports" (dict (el "new"(dict t))))) 1)) in
                        let () = Printf.printf "bridges %s -> (%s)\n%!" uuid name in 
                        Hashtbl.add c.db_br name (uuid, interfaces)
                        ) bridges)
            | "Open_vSwitch" -> begin
                    c.ovs_uuid <-  
                        (match dict a with
                        | [(ovs_uuid, _)] -> ovs_uuid 
                        | _ -> failwith "Failed to parse Open_vSwitch entry response")
            end
            | _ -> ()

        ) (dict resp) in
    return ()

let db_invoke_call c m p = 
    let id = !(c.dbid_uid) in 
    let () = c.dbid_uid := Int32.succ !(c.dbid_uid) in
    let buf = json_to_string (`O [
        ("method", (`String m));
        ("params", p);
        ("id", (`Float (Int32.to_float id)))
    ]) in
    let () = Printf.printf "<< %s (%d)\n%!" buf (String.length buf) in 
    let (t, u) = Lwt.wait () in 
    let _ = Hashtbl.add c.db_resp id u in
    lwt _ = Lwt_unix.write_string c.db_fd buf 0 (String.length buf) in
    lwt r = t in
    let _ = Hashtbl.remove c.db_resp id in
    return r

let db_process_call c xid name resp = 
    match name with
    | "echo" ->
            let buf = json_to_string 
            (`O [("id", xid); ("result", (`A [])); ("error", `Null) ]) in
            lwt _ = Lwt_unix.write_string c.db_fd buf 0 (String.length buf) in 
            return ()
    | "update" -> 
            db_update_state c (List.nth (enum resp) 1)
    | _ -> return (Printf.printf "%% ERROR DB Unsupported %s\n%!" name) 

let db_process_msg c m =
    let m = dict m in
    if (List.exists (fun (n, _) -> n = "method" ) m) then
        let name = str (el "method" m) in
        let params = el "params" m in 
        let id = el "id" m in
        db_process_call c id name params
    else begin
        if (List.exists (fun (n, _) -> n = "result" ) m) then
            let id = intj (el "id" m) in
            let _ =
                if (Hashtbl.mem c.db_resp id) then
                    Lwt.wakeup (Hashtbl.find c.db_resp id) (`O m)
            in
                return ()
        else 
            return (Printf.printf "ERROR DB unsupported %s\n%!" (json_to_string (`O m)))
    end



let db_loop (c:t) =
    let rec db_loop_inner (c:t) =
        let buf = Bytes.make 4096 '\000' in
        lwt len = Lwt_unix.read c.db_fd buf 0 4096 in
        let buf = Bytes.sub_string buf 0 len in
        let rec process_buf c buf =
            match (json_of_src  (`String buf)) with
            | (l, `JSON a) ->
                let _ = Printf.printf ">> %s\n%!" (json_to_string a) in
                lwt _ =  db_process_msg c a in
                process_buf c (String.sub buf l ((String.length buf)-l) )
            | (_, `Error _) -> return ()
        in
        lwt _ = process_buf c buf in
        db_loop_inner c
    in
    try_lwt 
        db_loop_inner c
    with e ->
        return (Printf.printf "ERR DB %s\n%!" (Printexc.to_string e))

let init_controller ?ovsdb_port:(ovsdb_port=6634) host = 
    let db_fd = Lwt_unix.of_unix_file_descr (Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0) in
    let sockaddr = Unix.ADDR_INET((Unix.inet_addr_of_string (Ipaddr.V4.to_string host)), ovsdb_port) in
    let () = Lwt_unix.setsockopt db_fd Unix.TCP_NODELAY true in
    lwt () = Lwt_unix.connect db_fd sockaddr in

    let c = {db_fd; db_resp=(Hashtbl.create 64); dbid_uid=(ref 0l); db_br=(Hashtbl.create 64); 
                db_port=(Hashtbl.create 64);ovs_uuid="";}  in 
    (* run daemons on the background *)
    let _ = Lwt.ignore_result (db_loop c) in
    lwt resp =  db_invoke_call c "monitor"
            (`A [
                (`String "Open_vSwitch"); `Null;
                (`O ["Bridge", (`O ["columns", (`A [(`String "name"); (`String "ports")])]);
                ("Port", (`O ["columns", (`A [(`String "name"); (`String "interfaces")])]));
                 ("Open_vSwitch", (`O ["columns", (`A [(`String "cur_cfg")])]));]);
            ]) in 

    lwt _ = db_update_state c (el "result" (dict resp)) in 
    return c

let db_bridge_interfaces c name = 
    let (_, interfaces) = Hashtbl.find c.db_br name in
    (List.map (fun a -> ((`A [(`String "uuid"); (`String a)]) )) interfaces )

let db_add_tunnel c br endpoint name domid = 
   (* Send a big transaction to set up the new tunnel across the databse tables *)
    Printf.printf "looking for %s\n%!" br;
    let (br_uuid, _) = Hashtbl.find c.db_br br in
    lwt resp =  db_invoke_call c "transact"
            (`A [
                (`String "Open_vSwitch");
                (`O [("op", (`String "insert")); 
                       ("table", (`String "Interface"));
                       ("row", (`O [
                           ("type", (`String "gre")); 
                           ("name", (`String name));
                           ("options", (`A [
                               (`String "map");
                               (`A [ (`A [(`String "remote_ip");(`String (Ipaddr.V4.to_string endpoint))]); (`A [(`String "key");(`String (string_of_int domid ) )])])
                               ]))
                       ]));
                       ("uuid-name",(`String "new_iface"));
                       ]);
                (`O [
                    ("op", (`String "insert")); 
                    ("table", (`String "Port")); 
                    ("uuid-name", (`String "new_port")); 
                    ("row", (`O [
                        ("name", (`String name)); 
                        ("interfaces", (`A [(`String "named-uuid");(`String "new_iface")]))
                    ]));
                ]);
                (`O [
                    ("op", (`String "update")); 
                    ("table", (`String "Bridge"));
                    ("where", (`A [
                        (`A [ 
                            (`String "_uuid"); (`String "=="); 
                            (`A [(`String "uuid"); (`String br_uuid)])
                        ])
                    ]));
                    ("row", 
                        (`O[("ports", (`A [(`String "set"); 
                        (`A ((`A [(`String "named-uuid"); (`String "new_port")]) :: (db_bridge_interfaces c br)) );]))]))
                ]);
                (`O [
                    ("table", (`String "Open_vSwitch"));
                    ("op", (`String "mutate"));
                    ("mutations", (`A [(`A [(`String "next_cfg"); (`String "+="); (`Float 1.0)])]));
                    ("where", (`A [
                        (`A [ 
                            (`String "_uuid"); (`String "=="); 
                            (`A [(`String "uuid"); (`String c.ovs_uuid)])
                        ])
                    ]));
                ]);
                (`O [("op", (`String "comment")); ("comment", (`String "jitsu adds new port"))])
            ])
    in
    try_lwt  
        let error = str (el "details" (dict (List.find
            (fun a -> el_ex "error" (dict a))
            (enum (el "result" (dict resp)))))) in
        failwith error
    with Not_found -> return ()

let db_del_tunnel c br _ name _ = 
    (* Send a big transaction to set up the new tunnel across the databse tables *)
    let () = Printf.printf "looking for %s %s \n%!" br name in 
    let (port_uuid, _) = Hashtbl.find c.db_port name in 
    let (br_uuid, interfaces) = Hashtbl.find c.db_br br in
    let interfaces = 
        List.filter (fun u -> u <> port_uuid) interfaces in
    let () = Hashtbl.replace c.db_br br (br_uuid, interfaces) in
    lwt _ =  db_invoke_call c "transact"
            (`A [
                (`String "Open_vSwitch");
                (`O [
                    ("op", (`String "update")); 
                    ("table", (`String "Bridge"));
                    ("where", (`A [
                        (`A [ 
                            (`String "_uuid"); (`String "=="); 
                            (`A [(`String "uuid"); (`String br_uuid)])
                        ])
                    ]));
                    ("row", 
                        (`O[("ports", (`A [(`String "set"); (`A (db_bridge_interfaces c br);)]))])
                    )
                ]);
                (`O [
                    ("table", (`String "Open_vSwitch"));
                    ("op", (`String "mutate"));
                    ("mutations", (`A [(`A [(`String "next_cfg"); (`String "+="); (`Float 1.0)])]));
                    ("where", (`A [
                        (`A [ 
                            (`String "_uuid"); (`String "=="); 
                            (`A [(`String "uuid"); (`String c.ovs_uuid)])
                        ])
                    ]));
                ]);
                (`O [("op", (`String "comment")); ("comment", (`String "jitsu adds new port"))])
            ])
    in
    return ()


