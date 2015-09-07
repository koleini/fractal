(*
 * Copyright (c) 2014-2015 Magnus Skjegstad <magnus@skjegstad.com>
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
open Cmdliner
open Irmin_unix

module Store = Irmin.Basic (Irmin_http.Make) (Irmin.Contents.String)
module Lock = Irmin.Private.Lock.Make(Store.Tag)
module View = Irmin.View (Store)

let info =
  let doc =
    "Just-In-Time Summoning of Unikernels. Jitsu is a forwarding DNS server \
     that automatically starts unikernel VMs when their domain is requested. \
     The DNS response is sent to the client after the unikernel has started, \
     enabling the client to use unmodified software to communicate with \
     unikernels that are started on demand. If no DNS requests are received \
     for the unikernel within a given timeout period, the VM is automatically \
     stopped." in
  let man = [
    `S "EXAMPLES";
    `P "jitsu -c xen:/// -f 8.8.8.8 -m destroy mirage.org,10.0.0.1,mirage-www";
    `P "Connect to Xen. Start VM 'mirage-www' on requests for mirage.org and \
        return IP 10.0.0.1 when VM is running. Forward unknown requests to \
        8.8.8.8 (Google). Expired VMs are destroyed.";
    `P "jitsu -c vbox:///session -m suspend home.local,192.168.0.1,ubuntu -t 60";
    `P "Connect to Virtualbox. Start VM 'ubuntu' on requests for home.local \
        and return IP 192.168.0.1. Forward unknown requests to system default. \
        Expired VMs are suspended after 120 seconds (2 x DNS ttl).";
    `S "AUTHORS";
    `P "Magnus Skjegstad <magnus@skjegstad.com>" ;
    `S "BUGS";
    `P "Submit bug reports to http://github.com/magnuss/jitsu";] in
  Term.info "jitsu" ~version:"0.2-alpha" ~doc ~man

let bindaddr =
  let doc = "Bind local DNS server to interface with this IP" in
  Arg.(value & opt string "127.0.0.1" & info ["b"; "bind"] ~docv:"ADDR" ~doc)

let bindport =
  let doc = "UDP port to listen for DNS queries" in
  Arg.(value & opt int 53 & info ["l"; "listen"] ~docv:"PORT" ~doc)

let connstr =
  let doc =
    "libvirt connection string (e.g. xen+ssh://x.x.x.x/system or vbox:///session)"
  in
  Arg.(value & opt string "xen:///" & info ["c"; "connect"] ~docv:"CONNECT" ~doc)

let forwarder =
  let doc =
    "IP address of DNS server queries should be forwarded to if no local match \
     is found. Defaults to system default (/etc/resolv.conf) if not specified. \
     Set to 0.0.0.0 to disable forwarding."
  in
  Arg.(value & opt string "" & info ["f" ; "forwarder"] ~docv:"ADDR" ~doc)

let forwardport =
  let doc = "UDP port to forward DNS queries to" in
  Arg.(value & opt int 53 & info ["p"; "port"] ~docv:"PORT" ~doc)

let response_delay =
  let doc =
    "Time to wait in seconds before responding to a DNS query after the local \
     VM has started. This delay gives the VM time to open up the necessary TCP \
     ports etc. Setting this value too low can result in long delays on the \
     first TCP request to the VM." in
  Arg.(value & opt float 0.1 & info ["d" ; "delay" ] ~docv:"SECONDS" ~doc)

let map_domain =
  let doc =
    "Maps DOMAIN to a VM and IP. VM must match a VM available through libvirt \
     (see virsh list --all)." in
  Arg.(non_empty & pos_all (t3 ~sep:',' string string string) [] & info []
         ~docv:"DOMAIN,IP,VM" ~doc)

let ttl =
  let doc =
    "DNS TTL in seconds. The TTL determines how long the clients may cache our \
     DNS response. VMs are terminated after no DNS requests have been received \
     for TTL*2 seconds." in
  Arg.(value & opt int 60 & info ["t" ; "ttl" ] ~docv:"SECONDS" ~doc)

let vm_stop_mode =
  let doc =
    "How to stop running VMs after timeout. Valid options are $(b,suspend), \
     $(b,destroy) and $(b,shutdown). Suspended VMs are generally faster to \
     resume, but require resources to store state. Note that Mirage \
     suspend/resume is currently not supported on ARM." in
  Arg.(value & opt (enum [("destroy" , Vm_stop_mode.Destroy);
                          ("suspend" , Vm_stop_mode.Suspend);
                          ("shutdown", Vm_stop_mode.Shutdown)])
         Vm_stop_mode.Shutdown & info ["m" ; "mode" ] ~docv:"MODE" ~doc)

let synjitsu_domain_uuid =
  let doc =
    "UUID or domain name of a Synjitsu compatible unikernel. When specified, \
     Jitsu will attempt to connect to this domain over Vchan on port 'synjitsu' \
     and send notifications with updates on MAC- and IP-addresses of booted \
     unikernels. This allows Synjitsu to send gratuitous ARP on behalf of \
     booting unikernels and to cache incoming SYN packets until they are \
     ready to receive them."  in
  Arg.(value & opt (some string) None & info ["synjitsu"] ~docv:"NAME_OR_UUID" ~doc)

let log m =
  print_endline (Printf.sprintf "\027[36m %s\027[m%!" m)

let or_abort f =
  try f () with
  | Failure m -> (Printf.fprintf stderr "Fatal error: %s" m); exit 1

let or_warn msg f =
  try f () with
  | Failure m -> (log (Printf.sprintf "Warning: %s\nReceived exception: %s" msg m)); ()

let or_warn_lwt msg f =
  try f () with
  | Failure m -> (log (Printf.sprintf "Warning: %s\nReceived exception: %s" msg m)); Lwt.return_unit

let backend =
  let doc =
    "Which backend to use. Currently only libvirt is supported." in
  Arg.(value & opt (enum [("libvirt" , `Libvirt);
                          ("libxl", `Libxl);
                          ("xapi", `Xapi)])
         `Libvirt & info ["x" ; "backend" ] ~docv:"BACKEND" ~doc)

let jitsu backend connstr bindaddr bindport forwarder forwardport response_delay 
    map_domain ttl vm_stop_mode use_synjitsu =
  let (module B) = 
      if backend = `Libvirt then 
          (module Libvirt_backend : Backends.VM_BACKEND) 
      else if backend = `Xapi then
          (module Xapi_backend : Backends.VM_BACKEND)
      else
      (module Libvirt_backend) 
  in
  let module Jitsu = Jitsu.Make(B) in
  Lwt_main.run (
    ((match forwarder with
        | "" -> Dns_resolver_unix.create () >>= fun r -> (* use resolv.conf *)
          Lwt.return (Some r)
        | "0.0.0.0" -> Lwt.return None
        | _  -> let forwardip = Ipaddr.of_string_exn forwarder in (* use ip from forwarder *)
          let servers = [(forwardip,forwardport)] in
          let config = `Static ( servers , [""] ) in
          Dns_resolver_unix.create ~config:config () >>= fun r ->
          Lwt.return (Some r)
      )
     >>= fun forward_resolver ->
     log (Printf.sprintf "Connecting to %s...\n" connstr);
     B.connect connstr
     >>= fun r ->
     match r with
     | `Error _ -> raise (Failure "Unable to connect to backend") 
     | `Ok backend_t ->

       let rec maintenance_thread t timeout =
         Lwt_unix.sleep timeout >>= fun () ->
         or_warn_lwt "Unable to stop expired VMs" (fun () -> Jitsu.stop_expired_vms t) >>= fun () ->
         maintenance_thread t timeout;
       in

       or_abort (fun () -> Jitsu.create backend_t log forward_resolver ~use_synjitsu ()) >>= fun t ->
      
       let watch_remote_hosts ~storage ~host:_r_host diff =
         let process_response () =
           log "xen response detected";
           Irmin_backend.get_response storage ~vm_name:Config.l_host.Config.name >>= function
            | Rpc.Enum response when response <> [] ->
              (* read response params *)
              return ()
            | _ -> (* raise (Failure "Empty or non well-formed response") *)
              return ()
         in
         match diff with
         | `Added _  | `Updated _ -> process_response ()
         | `Removed _ -> return ()
       in
       let process_tags config _tag diff =
        
         let process_command key value =
           match (List.nth key 1) with
           | "action" -> begin 
             let vm_name  = List.nth key 0 in
             match Config.rpc_of_string value with
             | Rpc.Enum action when action <> [] ->
			         let params = List.tl action in begin
			         match Rpc.string_of_rpc (List.hd action) with
			         | "add_vm" -> Jitsu.add_replica t ~vm_name ~params
               | "del_vm" -> Jitsu.del_replica t ~vm_name ~params
               | "def_vm" ->
                 let kernel  = Uri.of_string (Rpc.string_of_rpc (List.nth params 0)) in
                 let mac = Macaddr.of_string_exn (Rpc.string_of_rpc (List.nth params 1)) in
			           B.define_vm backend_t ~name_label:vm_name ~mAC:mac ~pV_kernel:kernel >>=
                 fun _ -> return ()
               | act ->
                 return (log (Printf.sprintf "requested action (%s) is not valid" act))
               end
             | _ -> raise (Failure (Printf.sprintf "Empty or non well-formed request: %s\n" value))
           end
           | _ -> return (log (Printf.sprintf "key: %s added: %s" (String.concat "/" key) value))
         in
         let process_diff view_p h =
           lwt s = Store.of_head config task h in
           lwt view = View.of_path (s "of-path views") ["jitsu"; "request";] in
           View.diff view_p view >>= fun views ->
           let _ =
           Lwt_list.iter_s (fun (k, v) ->
             match v with
             | `Added v -> process_command k v
             | `Updated (_, v) -> process_command k v
             | `Removed v -> return (log (Printf.sprintf "key: %s removed: %s" (String.concat " " k) v))
             ) views
           in
             return ()
          in 
          match diff with
          | `Added h ->
              lwt view_p = View.empty () in
              process_diff view_p h
          | `Updated (h1, h2) -> 
              lwt s = Store.of_head config task h1 in
              lwt view_p = View.of_path (s "of-path views") ["jitsu"; "request";] in
              process_diff view_p h2
          | `Removed _ -> return (log "keys removed")
       in
       let storage = Jitsu.get_storage t in
       let ir_conn = Irmin_backend.get_irmin_conn storage in
       Lwt_list.iter_s (fun h ->
         Irmin_backend.create ~address:h.Config.irmin_store () >>= fun s ->
         let ir_conn = Irmin_backend.get_irmin_conn s in
         let path = [ "jitsu" ; "request"; Config.l_host.Config.name; "response"; ] in
         Irmin.update (ir_conn "Add response") path "[]" >>
         Irmin.watch_key (ir_conn "jitsu") path
           (watch_remote_hosts ~storage:s ~host:h) >>= fun _ ->
             return ()
         ) Config.r_hosts >>
         Irmin.watch_tags (ir_conn "jitsu")
           (process_tags (Irmin_http.config Config.l_host.Config.irmin_store)) >>= fun _ ->   

       Lwt.choose [(
           (* main thread, DNS server *)
           let add (dns_name,vm_ip,vm_name) =
             log (Printf.sprintf "Adding domain '%s' for VM '%s' with ip %s\n" dns_name vm_name vm_ip);
             or_abort (fun () -> Jitsu.add_vm t ~dns_names:[Dns.Name.of_string dns_name] ~vm_name
                 ~vm_ip:(Ipaddr.V4.of_string_exn vm_ip) ~vm_stop_mode ~response_delay ~dns_ttl:ttl)
           in
           Lwt_list.iter_p add map_domain
           >>= fun () ->
           log (Printf.sprintf "Starting server on %s:%d...\n" bindaddr bindport);
           let processor = ((Dns_server.processor_of_process (Jitsu.process t))
                            :> (module Dns_server.PROCESSOR)) in
           Dns_server_unix.serve_with_processor ~address:bindaddr ~port:bindport
             ~processor);
          (* maintenance thread, delay in seconds *)
          (* (maintenance_thread t 10.0); *)
          ]);
  )

let jitsu_t =
  Term.(pure jitsu $ backend $ connstr $ bindaddr $ bindport $ forwarder $ forwardport
        $ response_delay $ map_domain $ ttl $ vm_stop_mode $ synjitsu_domain_uuid )

let () =
  match Term.eval (jitsu_t, info) with
  | `Error _ -> exit 1
  | _ -> exit 0
