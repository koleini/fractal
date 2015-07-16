(*
 * Testing if the of library does the job for us
 * *)
open NaasLoad
open Lwt

let main () =
    try_lwt 
        lwt src_c = create_controller (Ipaddr.V4.of_string_exn "10.20.0.9") in
        lwt dst_c = create_controller (Ipaddr.V4.of_string_exn "10.20.0.101") in
(*        lwt _ = add_app_of13 src_c 1l (Ipaddr.V4.of_string_exn "10.20.0.94") 
                (Macaddr.of_string_exn "1e:77:f0:b4:bf:23") 1  
                (Ipaddr.V4.of_string_exn "10.20.0.94") in 
       lwt _ = add_replica_of13 src_c src_c 1l (Ipaddr.V4.of_string_exn "10.20.0.97") 
                (Macaddr.of_string_exn "ba:bc:41:78:5a:ab") 2 in
        lwt _ = add_replica_of13 src_c dst_c 1l (Ipaddr.V4.of_string_exn "10.20.0.99") 
                (Macaddr.of_string_exn "1e:f8:92:18:6b:06") 101 in  *)
(*      lwt _ = disable_replica_of13 src_c src_c 1l (Ipaddr.V4.of_string_exn "10.20.0.97") 2 in
        lwt _ = disable_replica_of13 src_c dst_c 1l (Ipaddr.V4.of_string_exn "10.20.0.99") 101 in  *)
      lwt _ = delete_replica_of13 src_c src_c 1l (Ipaddr.V4.of_string_exn "10.20.0.97") 2 in 
        lwt _ = delete_replica_of13 src_c dst_c 1l (Ipaddr.V4.of_string_exn "10.20.0.99") 101 in
        NaasLoad.wait src_c
    with ex -> 
        return (Printf.printf "[ERR] %s\n%!" (Printexc.to_string ex))

let () =
    Lwt_main.run( main ())
