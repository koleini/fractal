(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2014-2016 Masoud Koleini <masoud.koleini@nottingham.ac.uk>
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)
open Lwt
open Irmin_unix

(* Structure
 * Jitsu
 *      \-vm (vm/dns configuration)
 *          \-[uuid]
 *              \-dns
 *                  \-[dns_name]
 *                      \-ttl
 *                      (+ other stats)
 *              \-config (this config is read by the virt. backend and contains optional parameters)
 *                  \ ...
 *              \-stop_mode
 *              \-response_delay (+ other stats)
 *              \-ip
 *     \-stats (various dynamic stats)
 *          \-[uuid]
 *              \-dns
 *          ...
 *     \-macs
 *          \-[mac_address]
 *              \-[uuid]
*)


module Make (B : Irmin.S_MAKER) = struct
  module I = Irmin_unix.Irmin_http.Make(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

  type t = {
    connection : string -> I.t;
    log : (string -> unit);
    mutable dns_cache : Dns.Loader.db;
    mutable dns_cache_dirty : bool;
  }

  type id = string

  let get_irmin_conn t = t.connection

  let get_float t path  =
    I.read t path >>= fun r ->
    match r with
    | None -> Lwt.return_none
    | Some s -> Lwt.return (Some (float_of_string s))

  let set_float t path f =
    I.update t path (string_of_float f)

  let default_log msg =
    Printf.printf "irmin_backend: %s\n" msg

  let create ?persist:(persist=true) ?root:(root="irmin/test") ?log:(log=default_log) ?address () =
    let task msg = Irmin.Task.create ~date:(Int64.of_float (Unix.gettimeofday ())) ~owner:"jitsu" msg in
    let add =
      match address with
      | None -> failwith "Irmin connection has to be http"
      | Some add -> add
    in
    let config = Irmin_http.config add  in
    I.Repo.create config >>= I.master task >>= fun connection ->
    let dns_cache = Dns.Loader.new_db () in (* Start with empty DNS db *)
    let dns_cache_dirty = false in
    Lwt.return { connection ; log ; dns_cache_dirty ; dns_cache }

  let add_vm_dns t ~vm_uuid ~dns_name ~dns_ttl =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; "dns" ; (Dns.Name.to_string dns_name) ] in
    I.update (it "Registering domain ttl")      (path @ [ "ttl" ]) (string_of_int dns_ttl) >>= fun () ->
    t.dns_cache_dirty <- true;
    Lwt.return_unit

  let delete_vm t ~vm_uuid ~vm_mac =
    let it = t.connection in
    let path = [ "jitsu" ; ] in
    I.remove (it "Removing vm data") ( path @ [ "vm" ; (Uuidm.to_string vm_uuid) ; ] ) >>= fun () ->
    I.remove (it "Removing vm status") ( path @ [ "status" ; (Uuidm.to_string vm_uuid) ; ] ) >>= fun () ->
    I.remove (it "Removing vm mac") ( path @ [ "macs" ; (Macaddr.to_string vm_mac) ; ] )

  let list_of_hashtbl hashtbl =
    (* fold Hashtbl into (key, value list) list, where the list of values is all bindings for this key in the hash table.
     * The list of values is in inserted order, the key order is unspecified *)
    let keys = Hashtbl.fold (fun k _ l ->
        match (List.exists (fun s -> s = k) l) with (* fold hashtbl to list of unique keys*)
        | false -> l @ [k]
        | true -> l)
        hashtbl [] in
    List.fold_left (fun l key ->
        (* values returned from find_all is in reversed inserted order, so reverse list *)
        let bindings = List.rev (Hashtbl.find_all hashtbl key) in
        let values = List.fold_left (fun l v -> l @ [v]) [] bindings in
        l @ [(key, values)]
      ) [] keys

  let hashtbl_of_list lst =
    (* Insert key/values in Hastbl. List is expected to be in format (key, value list) list, as returned by list_of_hashtbl. *)
    let tbl = Hashtbl.create (List.length lst) in (* this length will be wrong if there are multiple bindings per key *)
    List.iter (fun row ->
        let key,value_list = row in
        List.iter (fun v ->
            Hashtbl.add tbl key v) value_list) lst;
    tbl

  let add_vm t ~vm_uuid ~vm_ip ~vm_stop_mode ~response_delay ~wait_for_key ~use_synjitsu ~vm_config =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.update (it "Registering VM stop mode")       (path @ [ "stop_mode" ]) (Vm_stop_mode.to_string vm_stop_mode) >>= fun () ->
    I.update (it "Registering VM response delay")  (path @ [ "response_delay" ]) (string_of_float response_delay) >>= fun () ->
    I.update (it "Registering VM IP")              (path @ [ "ip" ]) (Ipaddr.V4.to_string vm_ip) >>= fun () ->
    let wait_for_key =
      match wait_for_key with
      | None -> ""
      | Some s -> s
    in
    I.update (it "Registering VM Xenstore wait key")    (path @ [ "wait_for_key" ]) wait_for_key >>= fun () ->
    I.update (it "Registering VM Synjitsu mode")    (path @ [ "use_synjitsu" ]) (string_of_bool use_synjitsu) >>= fun () ->
    let path = path @ [ "config" ] in
    let config_list = list_of_hashtbl vm_config in
    Lwt_list.iter_s (fun row ->
        let k,value_list = row in
        Lwt_list.iteri_s (fun i v ->
            I.update (it (Printf.sprintf "Registering extra config value %s (%d)" k i))
              (path @ [ k ; (string_of_int i) ]) v
          ) value_list)
      config_list
    >>= fun () ->
    t.dns_cache_dirty <- true;
    Lwt.return_unit

  let get_stop_mode t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get stop mode") (path @ [ "stop_mode" ]) >>= fun r ->
    match r with
    | None -> Lwt.return Vm_stop_mode.Unknown
    | Some s -> Lwt.return (Vm_stop_mode.of_string s)

  let set_stop_mode t ~vm_uuid stop_mode =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.update (it "Set stop mode") (path @ [ "stop_mode" ]) (Vm_stop_mode.to_string stop_mode)

  let get_use_synjitsu t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get VM Synjitsu mode") (path @ [ "use_synjitsu" ]) >>= fun r ->
    match r with
    | None -> Lwt.return false
    | Some s ->
      try
        if s = "1" then Lwt.return_true else
        if s = "0" then Lwt.return_false else
          Lwt.return (bool_of_string s)
      with
      | Invalid_argument _ -> Lwt.return_false

  let get_wait_for_key t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get Xenstore wait key") (path @ [ "wait_for_key" ]) >>= fun r ->
    match r with
    | None -> Lwt.return_none
    | Some s -> if s = "" then Lwt.return_none else Lwt.return (Some s)

  let get_ip t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get VM IP") (path @ [ "ip" ]) >>= fun r ->
    match r with
    | None -> Lwt.return_none
    | Some s -> Lwt.return (Ipaddr.V4.of_string s)

  let set_ip t ~vm_uuid ip =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.update (it "Set VM IP") (path @ [ "ip" ]) (Ipaddr.V4.to_string ip) >>= fun () ->
    t.dns_cache_dirty <- true;
    Lwt.return_unit

  let get_timestamp t ~vm_uuid ~label =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid)] in
    get_float (it ("Get VM timestamp for " ^ label)) (path @ [ label ])

  let set_timestamp t ~vm_uuid ~label ~time =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid); ] in
    set_float (it ("Set VM timestamp for " ^ label)) (path @ [ label ]) time

  let get_last_request_timestamp t ~vm_uuid ~dns_name =
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ; "dns" ; (Dns.Name.to_string dns_name)] in
    get_float (it "Get last request timestamp") (path @ [ "last_request_ts" ])

  let set_last_request_timestamp t ~vm_uuid ~dns_name last_request_ts =
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ; "dns" ; (Dns.Name.to_string dns_name)] in
    set_float (it "Set last request timestamp") (path @ [ "last_request_ts" ]) last_request_ts

  let get_start_timestamp t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ] in
    get_float (it "Get start timestamp") (path @ [ "start_ts" ])

  let set_start_timestamp t ~vm_uuid start_ts =
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ] in
    set_float (it "Set start timestamp") (path @ [ "start_ts" ]) start_ts

  let get_total_starts t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get total starts") (path @ [ "total_starts" ]) >>= fun r ->
    match r with
    | None -> Lwt.return 0
    | Some s -> Lwt.return (int_of_string s)

  let inc_total_starts t ~vm_uuid =
    (* TODO Should use transaction / view *)
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ] in
    get_total_starts t ~vm_uuid >>= fun starts ->
    I.update (it "Increase total starts") (path @ [ "total_starts" ]) (string_of_int (starts + 1))

  let get_total_requests t ~vm_uuid ~dns_name =
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ; "dns" ; (Dns.Name.to_string dns_name)] in
    I.read (it "Get total requests") (path @ [ "total_requests" ]) >>= fun r ->
    match r with
    | None -> Lwt.return 0
    | Some s -> Lwt.return (int_of_string s)

  let inc_total_requests t ~vm_uuid ~dns_name =
    (* TODO Should use transaction / view *)
    let it = t.connection in
    let path = [ "jitsu" ; "stats" ; (Uuidm.to_string vm_uuid) ; "dns" ; (Dns.Name.to_string dns_name) ] in
    get_total_requests t ~vm_uuid ~dns_name >>= fun total_requests ->
    I.update (it "Increase total requests") (path @ [ "total_requests" ]) (string_of_int (total_requests + 1))

  let get_ttl t ~vm_uuid ~dns_name =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; "dns" ; (Dns.Name.to_string dns_name) ] in
    I.read (it "Get DNS TTL") (path @ [ "ttl" ]) >>= fun r ->
    match r with
    | None -> Lwt.return 0
    | Some s -> Lwt.return (int_of_string s)

  let set_ttl t ~vm_uuid ~dns_name ttl =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; "dns" ; (Dns.Name.to_string dns_name) ] in
    I.update (it "Set DNS TTL") (path @ [ "ttl" ]) (string_of_int ttl) >>= fun () ->
    t.dns_cache_dirty <- true;
    Lwt.return_unit

  let get_response_delay t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; "response_delay" ] in
    get_float (it "Get VM response delay") path >>= fun d ->
    match d with
    | None -> Lwt.return 0.0
    | Some f -> Lwt.return f

  let set_response_delay t ~vm_uuid response_delay =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; "response_delay" ] in
    set_float (it "Set VM response delay") path response_delay

  let get_domid t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; ] in
    I.read (it "Get VM domain identifier") (path @ [ "domid" ]) >>= fun domid ->
    match domid with
    | None -> Lwt.return 0
    | Some s -> Lwt.return (int_of_string s)

  let set_domid t ~vm_uuid ~domid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; ] in
    I.update (it "Set VM domain identifier") (path @ [ "domid" ]) (string_of_int domid) >>= fun () ->
    Lwt.return_unit

  (** Get a list of sub-key names as strings from an Irmin path *)
  let get_key_names t path =
    let it = t.connection in
    I.list (it "Retrieving key list") path >>= fun key_list ->
    Lwt_list.filter_map_s (fun v ->
        match (Irmin.Path.String_list.rdecons v) with
        | None -> Lwt.return None
        | Some (_,key) -> Lwt.return (Some key)
      ) key_list

  let get_vm_config t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; "config" ] in
    get_key_names t path
    >>= fun config_keys ->
    Lwt_list.fold_left_s (fun l key ->
        get_key_names t (path @ [key]) >>= fun value_keys -> (* get value keys, expected to be ints 0-n *)
        let value_counter = List.sort (fun a b -> (Int64.compare (Int64.of_string a) (Int64.of_string b))) value_keys in (* sort by numeric order *)
        Lwt_list.fold_left_s (fun l i ->
            I.read (it (Printf.sprintf "Read config value %s (%s)" key i)) (path @ [ key ; i ]) >>= fun r ->
            match r with
            | None -> Lwt.return l
            | Some s -> Lwt.return (l @ [s])
          ) [] value_counter >>= fun values ->
        Lwt.return (l @ [(key, values)])
      ) [] config_keys
    >>= fun folded_hashtbl ->
    Lwt.return (hashtbl_of_list folded_hashtbl)

  let get_vm_list t =
    let path = [ "jitsu" ; "vm" ] in
    get_key_names t path >>= fun key_names ->
    Lwt_list.filter_map_s (fun v ->
        match (Uuidm.of_string v) with
        | None -> t.log (Printf.sprintf "Unable to parse UUID %s, VM ignored" v); Lwt.return_none
        | Some uuid -> Lwt.return (Some uuid)
      ) key_names

  let get_vm_name t vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get VM name") (path @ [ "config" ; "name" ; "0" ; ]) >>= fun n ->
    match n with
    | None -> Lwt.return ""
    | Some s -> Lwt.return s

  let replace_uuids t vm_uuid vm_uuid' =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; ] in
    let rec find_keys keys =
      match keys with
      | [] -> return []
      | h::t ->
        I.list (it "Retrieving key list") h >>= fun subh ->
        match subh with
        | [] -> find_keys t >>= fun r -> return (h::r)
        | h' -> find_keys h' >>= fun v1 -> find_keys t >>= fun v2 -> return (v1@v2)
    in
    find_keys [path] >>= fun key_list ->
    Lwt_list.iter_s (fun key ->
        I.read (it "Get VM name") key >>= fun v ->
        let value = match v with
        | None -> ""
        | Some value -> value
        in
          (* let path' = [ "jitsu" ; "vm" ; "OoO" ; ] @ (List.tl (List.tl (List.tl key))) in *)
          I.remove (it "Set VM domain identifier") key >>= fun () ->
          let path' = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid') ; ] @ (List.tl (List.tl (List.tl key))) in
          I.update (it "Set VM domain identifier") path' value >>= fun () ->
          (* Printf.printf "*** %s : %s " (Irmin.Path.String_list.to_hum key) value; *)
          return ();
      ) key_list
    >>= fun _ ->
    get_key_names t path >>= fun key_names ->
    Lwt_list.iter_s ( fun v ->
      Printf.printf "* %s -> %s\n" (Uuidm.to_string vm_uuid) v;
      return ()
    ) key_names

  let get_host_list t =
    let it = t.connection in
    let path = [ "jitsu" ; "datacenter" ] in
    get_key_names t path >>= fun names ->
    Lwt_list.map_s (fun n ->
      get_float (it "Get remote-host timestamp") (path @ [n]) >>= fun tm ->
      return (n, tm)
      ) names

  let get_uuid_of_mac t ~vm_mac =
    let it = t.connection in
    let path = [ "jitsu" ; "macs" ] in
    I.read (it "Get VM UUID of MAC") (path @ [ Macaddr.to_string vm_mac ]) >>= fun m ->
    match m with
    | None -> Lwt.return_none
    | Some s -> Lwt.return (Uuidm.of_string s)

  let set_uuid_for_mac t ~vm_mac ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "macs" ] in
    I.update (it "Set VM UUID for MAC") (path @ [ Macaddr.to_string vm_mac ]) (Uuidm.to_string vm_uuid)

  let get_vm_dns_name_list t ~vm_uuid =
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; "dns" ] in
    get_key_names t path >>= fun dns_names ->
    Lwt_list.map_s (fun name ->
        Lwt.return (Dns.Name.of_string name)
      ) dns_names

  let create_dns_db t =
    let dns_db = Dns.Loader.new_db () in
    get_vm_list t >>= fun vm_list ->
    Lwt_list.iter_s (fun vm_uuid ->
        t.log (Printf.sprintf "create_dns_db: found vm %s" (Uuidm.to_string vm_uuid));
        get_ip t ~vm_uuid >>= fun r ->
        match r with
        | None -> t.log (Printf.sprintf "create_dns_db: VM %s has no IP, skipping" (Uuidm.to_string vm_uuid)); Lwt.return_unit
        | Some ip ->
          get_vm_dns_name_list t ~vm_uuid >>= fun dns_name_list ->
          Lwt_list.iter_s (fun dns_name ->
              let base_domain = Dns_helpers.get_base_domain dns_name in
              let answer = Dns_helpers.has_local_domain dns_db base_domain Dns.Packet.Q_SOA in
              get_ttl t ~vm_uuid ~dns_name >>= fun ttl ->
              if not answer then (
                t.log (Printf.sprintf "create_dns_db: Adding SOA '%s' with ttl=%d" (Dns.Name.to_string base_domain) ttl);
                (* add soa if not registered before *) (* TODO use same ttl? *)
                Dns_helpers.add_soa dns_db base_domain ttl;
              );
              (* add dns record *)
              t.log (Printf.sprintf "create_dns_db: Adding A PTR for '%s' with ttl=%d and ip=%s" (Dns.Name.to_string dns_name) ttl (Ipaddr.V4.to_string ip));
              Dns.Loader.add_a_rr ip (Int32.of_int ttl) dns_name dns_db;
              Lwt.return_unit
            ) dns_name_list
          >>= fun () ->
          Lwt.return_unit
      )
      vm_list >>= fun () ->
    Lwt.return dns_db

  let rec get_dns_db t =
    match t.dns_cache_dirty with
    | true -> create_dns_db t >>= fun new_db ->
      t.dns_cache <- new_db;
      t.dns_cache_dirty <- false;
      get_dns_db t
    | false -> Lwt.return t.dns_cache

  let get_init_host t ~vm_uuid = (* M : host -> uuid *)
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get VM initial host") (path @ [ "init_host" ]) >>= fun r ->
    match r with
    | None -> Lwt.return (List.hd Dc_params.hosts_names)
    | Some s -> Lwt.return s

  let set_init_host t ~vm_uuid host =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; ] in
    I.update (it "Set VM initial host") (path @ [ "init_host" ]) host

  let get_init_uuid t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    print_endline (Printf.sprintf "get_init replica for uuid: %s" (Uuidm.to_string vm_uuid));
    I.read (it "Get VM initial UUID") (path @ [ "init_uuid" ]) >>= fun r ->
    match r with
    | None -> print_endline "END of get_init... (1)"; Lwt.return_none
    | Some s -> print_endline "END of get_init... (2)"; Lwt.return (Uuidm.of_string s)

  let set_init_uuid t ~vm_uuid init_vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; ] in
    I.update (it "Set initial VM") (path @ [ "init_uuid" ]) (Uuidm.to_string init_vm_uuid)

  let get_vm_state t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get VM state") (path @ [ "state" ]) >>= fun r ->
    match r with
    | None -> Lwt.return Vm_state.Unknown
    | Some s -> Lwt.return (Vm_state.of_string s)

  let set_vm_state t ~vm_uuid state =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ; ] in
    I.update (it "Set VM state") (path @ [ "state" ]) (Vm_state.to_string state)

  let get_response t ~requestor =
    let it = t.connection in
    let path = [ "jitsu" ; "request" ; requestor ] in
    I.read (it "Get response") (path @ [ "response" ]) >>= fun r ->
    match r with
    | None -> Lwt.return (Rpc.Enum [])
    | Some s -> Lwt.return (Dc_params.rpc_of_string s)

  let set_response t ~requestor ~response =
    let it = t.connection in
    let path = [ "jitsu" ; "request" ; requestor ] in
    I.update (it "Set response") (path @ [ "response" ]) (Rpc.to_string response)

  let get_appid t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get VM application identifier") (path @ [ "appid" ]) >>= fun r ->
    match r with
    | None -> Lwt.return 0
    | Some s -> Lwt.return (int_of_string s)

  let set_appid t ~vm_uuid ~appid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.update (it "Set VM application number") (path @ [ "appid" ]) (string_of_int appid)

  let get_nor t ~vm_uuid =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.read (it "Get VM number of replicas") (path @ [ "num_of_reps" ]) >>= fun num ->
    match num with
    | None -> Lwt.return 0
    | Some n -> Lwt.return (int_of_string n)

  let set_nor t ~vm_uuid ~num =
    let it = t.connection in
    let path = [ "jitsu" ; "vm" ; (Uuidm.to_string vm_uuid) ] in
    I.update (it "Set VM number of replicas") (path @ [ "num_of_reps" ]) (string_of_int num)

  let request_replica t ~init_vm_uuid ~init_host ~appid ~vm_stop_mode ~kernel =
    let it = t.connection in
    let path = [ "jitsu" ; "request" ; (List.hd Dc_params.hosts_names) ] in
    let rpc = Rpc.Enum [
        Rpc.rpc_of_string "add";
        Rpc.rpc_of_string (Uuidm.to_string init_vm_uuid);
        Rpc.rpc_of_string init_host;
        Rpc.rpc_of_int appid;
        Rpc.rpc_of_string (Vm_stop_mode.to_string vm_stop_mode);
        Rpc.rpc_of_string (Uri.to_string kernel);
        Rpc.rpc_of_string (string_of_float (Unix.time ()));
      ]
    in
    I.update (it "Registering replica action") (path @ [ "action"; ]) (Rpc.to_string rpc)

end
