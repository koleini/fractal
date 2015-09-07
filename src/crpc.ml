(* config in main, or use a pre-defined lis of xenservers *)
let server = "127.0.0.1"
let port = 5006

let socket =
  let server_addr =
    try Unix.inet_addr_of_string server
    with Failure("inet_addr_of_string") -> 
      try  (Unix.gethostbyname server).Unix.h_addr_list.(0) 
      with Not_found ->
        failwith (Printf.sprintf "%s : Unknown server\n" server)
  in Unix.ADDR_INET(server_addr, port) 

let check_cmd_args args count = 
  if ((Array.length args) < (2 + count)) then
    failwith (Printf.sprintf "Insufficient args for command %s (required %d)"
              args.(1) count)

let c_function args (ic, oc) =
  try_lwt
    if ((Array.length args) < 2) then
      failwith "No command defined";

    lwt resp =
      let params =
        match (args.(1)) with
        | "define_vm" -> 
             let _ = check_cmd_args args 2 in
               [ Rpc.String args.(2);
                 Rpc.String args.(3);
                 Rpc.String args.(4);
               ]
        | "add_replica" -> 
             let _ = check_cmd_args args 4 in
               [ Rpc.String args.(2);                (* vm_name *)
                 Rpc.Int (Int64.of_string args.(3)); (* app_number *)
                 Rpc.Int (Int64.of_string args.(4)); (* stop_mode *)
                 Rpc.Int (Int64.of_string args.(5)); (* ttl_time *)
                 Rpc.String args.(6);                (* kernel *)
                 Rpc.Bool (bool_of_string args.(7));  (* kernel *)
               ]
        | "del_replica" -> 
             let _ = check_cmd_args args 0 in
               [ Rpc.String args.(2);
               ]
        | _ -> 
            failwith (Printf.sprintf "Fail: unknown cmd: %s\n%!" args.(1))
      in
        let cmd = Rpc.({name=args.(1); params;}) in 
        lwt _ = Lwt_io.write_line oc (Jsonrpc.string_of_call cmd) in
        lwt read = Lwt_io.read_line ic in
        let read = Jsonrpc.response_of_string read in
        Lwt.return read (* ( "Rpc success: " ^ (string_of_bool read.Rpc.success) ^ "\n" ^
                 "Rpc contents: " ^
                 (Rpc.to_string read.Rpc.contents)
               ) *)
    in
    let r = "Rpc success: " ^ (string_of_bool resp.Rpc.success) ^ "\n" ^
                 "Rpc contents: " ^ (Rpc.to_string resp.Rpc.contents) in
    let _ = Printf.printf "result:\n%s\n%!" r in
      Lwt.return resp
  with  ex -> 
     Lwt.return (Rpc.failure (Rpc.String (Printexc.to_string ex)))
