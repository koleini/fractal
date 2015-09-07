(* configuration parameters   *)
(* requires per-instance edit *)

type host = {
  name : string;
  ip : Ipaddr.V4.t;
  irmin_store : Uri.t
}

let l_host = { name = "caelum-608-ipv4.cl.cam.ac.uk"; 
                ip = Ipaddr.V4.of_string_exn "10.20.0.101";  (* TODO: extract from DNS *)
                irmin_store = Uri.of_string  "http://127.0.0.1:8080"
             }
let r_hosts = [ 
                { name = "caelum-609-ipv4.cl.cam.ac.uk";
                  ip = Ipaddr.V4.of_string_exn "10.20.0.100";
                  irmin_store = Uri.of_string  "http://127.0.0.1:8081"
                }
              ]

let find_r_host_by_name name =
  List.find (fun h -> h.name = name) r_hosts

let ignore_time = 10.0

let mac_ip =
ref [(* (Macaddr.of_string_exn "12:43:3d:a3:d3:01", Ipaddr.V4.of_string_exn "10.20.0.21"); *)
     (Macaddr.of_string_exn "12:43:3d:a3:d3:02", Ipaddr.V4.of_string_exn "10.20.0.22");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:03", Ipaddr.V4.of_string_exn "10.20.0.23");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:04", Ipaddr.V4.of_string_exn "10.20.0.24");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:05", Ipaddr.V4.of_string_exn "10.20.0.25");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:06", Ipaddr.V4.of_string_exn "10.20.0.26");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:07", Ipaddr.V4.of_string_exn "10.20.0.27");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:08", Ipaddr.V4.of_string_exn "10.20.0.28");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:09", Ipaddr.V4.of_string_exn "10.20.0.29");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:0a", Ipaddr.V4.of_string_exn "10.20.0.30");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:0b", Ipaddr.V4.of_string_exn "10.20.0.31");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:0c", Ipaddr.V4.of_string_exn "10.20.0.32");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:0d", Ipaddr.V4.of_string_exn "10.20.0.33");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:0e", Ipaddr.V4.of_string_exn "10.20.0.34");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:0f", Ipaddr.V4.of_string_exn "10.20.0.35");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:10", Ipaddr.V4.of_string_exn "10.20.0.36");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:11", Ipaddr.V4.of_string_exn "10.20.0.37");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:12", Ipaddr.V4.of_string_exn "10.20.0.38");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:13", Ipaddr.V4.of_string_exn "10.20.0.39");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:14", Ipaddr.V4.of_string_exn "10.20.0.40");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:15", Ipaddr.V4.of_string_exn "10.20.0.41");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:16", Ipaddr.V4.of_string_exn "10.20.0.42");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:17", Ipaddr.V4.of_string_exn "10.20.0.43");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:18", Ipaddr.V4.of_string_exn "10.20.0.44");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:19", Ipaddr.V4.of_string_exn "10.20.0.45");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:1a", Ipaddr.V4.of_string_exn "10.20.0.46");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:1b", Ipaddr.V4.of_string_exn "10.20.0.47");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:1c", Ipaddr.V4.of_string_exn "10.20.0.48");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:1d", Ipaddr.V4.of_string_exn "10.20.0.49");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:1e", Ipaddr.V4.of_string_exn "10.20.0.50");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:1f", Ipaddr.V4.of_string_exn "10.20.0.51");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:20", Ipaddr.V4.of_string_exn "10.20.0.52");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:21", Ipaddr.V4.of_string_exn "10.20.0.53");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:22", Ipaddr.V4.of_string_exn "10.20.0.54");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:23", Ipaddr.V4.of_string_exn "10.20.0.55");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:24", Ipaddr.V4.of_string_exn "10.20.0.56");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:25", Ipaddr.V4.of_string_exn "10.20.0.57");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:26", Ipaddr.V4.of_string_exn "10.20.0.58");
     (Macaddr.of_string_exn "12:43:3d:a3:d3:27", Ipaddr.V4.of_string_exn "10.20.0.59");
     (* (Macaddr.of_string_exn "12:43:3d:a3:d3:28", Ipaddr.V4.of_string_exn "10.20.0.60"); *)
    ]

let rec rpc_of_string s =
  (* TODO: not a safe function.
     Rpc.Dict and ';' in names and values are not allowed.
     Int32 and Int64 are not distinguishable.
     Sanity check required *)
  if s = "N" then Rpc.Null else
  match String.get s 0, String.get s 1, String.get s ((String.length s) - 1) with
  | '[', _, ']'   -> Rpc.Enum (
                      let arr = Str.split (Str.regexp ";") (String.sub s 1 ((String.length s) - 2)) in
                      List.map (fun r ->
                        rpc_of_string r
                       ) arr
                     )
  | 'S', '(', ')' -> Rpc.rpc_of_string (String.sub s 2 ((String.length s) - 3))
  | 'F', '(', ')' -> Rpc.rpc_of_float  (float_of_string (String.sub s 2 ((String.length s) - 3)))
  | 'I', '(', ')' -> Rpc.rpc_of_int32  (Int32.of_string (String.sub s 2 ((String.length s) - 3)))
  | 'D', '(', ')' -> Rpc.rpc_of_dateTime (String.sub s 2 ((String.length s) - 3))
  | x, y, z -> raise (Failure (Printf.sprintf "Unable to extract RPC from string: %c %c %c" x y z))

