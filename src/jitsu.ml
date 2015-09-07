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
 * ACTION OF CONTRACT, NEGLIGENCE OR OCONNECTTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

open Lwt
open Dns

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = Printf.sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")
let _blue fmt   = Printf.sprintf ("\027[36m"^^fmt^^"\027[m") 

let first_replica = ref true

module Make (Vm_backend : Backends.VM_BACKEND) = struct
  module Synjitsu = Synjitsu.Make(Vm_backend)

  type t = {
    mutable dns_db : Loader.db;                       (* DNS database *)
    storage : Irmin_backend.t;
    log : string -> unit;                             (* Log function *) 
    vm_backend : Vm_backend.t;                        (* Backend type *)
    forward_resolver : Dns_resolver_unix.t option;    (* DNS to forward request to if no
                                                         local match *)
    synjitsu : Synjitsu.t option;
  }

  let get_storage t = t.storage

  let create vm_backend log forward_resolver ?use_synjitsu:(use_synjitsu=None) () =
    (* initialise synjitsu *)
    let synjitsu = match use_synjitsu with
      | Some domain -> let t = (Synjitsu.create vm_backend log domain "synjitsu") in
        ignore_result (Synjitsu.connect t);  (* connect in background *)
        Some t
      | None -> None
    in
    Irmin_backend.create ~address:Config.l_host.Config.irmin_store () >>= fun storage ->
    Lwt.return {
      dns_db = Loader.new_db ();
      storage;
      log; 
      vm_backend;
      forward_resolver = forward_resolver;
      synjitsu ;
    }
 
  let or_vm_backend_error msg fn t =
    fn t >>= function
    | `Error e -> begin
        match e with 
        | `Not_found -> raise (Failure (red "%s: Not found" msg))
        | `Disconnected -> raise (Failure (red "%s: Disconnected" msg))
        | `Unknown s -> raise (Failure (red "%s: %s" msg s))
      end
    | `Ok t -> return t

  let get_vm_name t vm =
    or_vm_backend_error "Unable to get VM name from backend" (Vm_backend.get_name t.vm_backend) vm

  let get_vm_state t vm =
    or_vm_backend_error "Unable to get VM state from backend" (Vm_backend.get_state t.vm_backend) vm

  let get_s t ~vm_name =
    (* TODO: we are assuming that all Irmins listen to port 8080, can we store URI? *) 
    Irmin_backend.get_initial_xs t.storage ~vm_name >>= fun initial_xs ->
    let _ = t.log (initial_xs ^ " : " ^ vm_name) in
    let i_xs = List.find (fun h -> h.Config.name = initial_xs) (Config.l_host::Config.r_hosts) in
    Irmin_backend.create ~address:i_xs.Config.irmin_store ()
    
  let get_num_of_reps t ~vm_name ~app_name =
    lwt storage = get_s t ~vm_name in
    Irmin_backend.get_num_of_reps storage ~vm_name:app_name

  let set_num_of_reps t ~vm_name ~app_name ~number =
    lwt storage = get_s t ~vm_name in
    Irmin_backend.set_num_of_reps storage ~vm_name:app_name ~num:number

  let get_timestamp t ~vm_name ~app_name ~label =
    lwt storage = get_s t ~vm_name in
    Irmin_backend.get_timestamp storage ~vm_name:app_name ~label >>= function
    | None -> return 0.0
    | Some t -> return t

  let set_timestamp t ~vm_name ~app_name ~label ~time =
    lwt storage = get_s t ~vm_name in
    Irmin_backend.set_timestamp storage ~vm_name:app_name ~label ~time

  let stop_vm t vm =
    get_vm_name t vm >>= fun vm_name ->
    get_vm_state t vm >>= fun vm_state ->
    match vm_state with
    | Vm_state.Running ->
        or_vm_backend_error "Unable to get MAC for VM" (Vm_backend.get_mac t.vm_backend) vm >>= fun vm_mac ->
        lwt () = match vm_mac with
          | Some mac -> begin
          (* configure open vswitch *)
            lwt domid = or_vm_backend_error "Unable to get domain id for VM" (Vm_backend.get_domain_id t.vm_backend) vm in
            Irmin_backend.get_app_id t.storage ~vm_name >>= fun app_id ->
            if app_id = 0 then
              raise (Failure (red "%s: application number unknown" vm_name))
            else
              Irmin_backend.get_ip t.storage ~vm_name >>= function
              | Some ip -> begin
                Irmin_backend.get_initial_xs t.storage ~vm_name >>= fun initial_xs ->
                  let i_xs = List.find (fun h -> h.Config.name = initial_xs) Config.r_hosts in
                  if (Macaddr.to_string mac) = vm_name then
                    Ovs_backend.del_replica app_id ip mac domid i_xs.Config.ip Config.l_host.Config.ip
                  else
                    Ovs_backend.del_application app_id ip mac domid Config.l_host.Config.ip
              end
            | None -> t.log (Printf.sprintf "VM %s has no IP. No bridge update applied." vm_name); return ()
            end
          | None -> return ()
        in
        Irmin_backend.get_stop_mode t.storage ~vm_name >>= fun stop_mode ->
        begin match stop_mode with
        | Vm_stop_mode.Unknown -> t.log (red "Unable to stop VM %s. Unknown stop mode requested\n" vm_name); 
          Lwt.return_unit
        | Vm_stop_mode.Shutdown -> t.log (yellow "VM shutdown: %s\n" vm_name);
          or_vm_backend_error "Unable to shutdown VM" (Vm_backend.shutdown_vm t.vm_backend) vm
        | Vm_stop_mode.Suspend  -> t.log (yellow "VM suspend: %s\n" vm_name);
          or_vm_backend_error "Unable to suspend VM" (Vm_backend.suspend_vm t.vm_backend) vm
        | Vm_stop_mode.Destroy  -> t.log (yellow "VM destroy: %s\n" vm_name) ; 
          or_vm_backend_error "Unable to destroy VM" (Vm_backend.destroy_vm t.vm_backend) vm
      end
    | Vm_state.Off
    | Vm_state.Paused
    | Vm_state.Unknown -> Lwt.return_unit (* VM already stopped or nothing we can do... *)


  let start_vm' t vm first initial_xs =
    get_vm_name t vm >>= fun vm_name ->
    get_vm_state t vm >>= fun vm_state ->
    t.log (green "Starting %s (%s)" vm_name (Vm_state.to_string vm_state));
    let update_stats () =
      Irmin_backend.set_start_timestamp t.storage ~vm_name (Unix.time ()) >>= fun () ->
      Irmin_backend.inc_total_starts t.storage ~vm_name
    in
    let notify_synjitsu () =
      Irmin_backend.get_ip t.storage ~vm_name >>= fun r ->
      match r with
      | Some ip -> begin
          or_vm_backend_error "Unable to get MAC for VM"
            (Vm_backend.get_mac t.vm_backend) vm >>= fun vm_mac ->
          match vm_mac with
          | Some mac -> begin
            (* configure open vswitch *)
            lwt _ =
              lwt domid = or_vm_backend_error "Unable to get domain id for VM"
                (Vm_backend.get_domain_id t.vm_backend) vm in
              Irmin_backend.get_app_id t.storage ~vm_name >>= fun app_id ->
              match first with
              | true  -> Ovs_backend.add_application app_id ip mac domid Config.l_host.Config.ip
              | false -> Ovs_backend.add_replica app_id ip mac domid initial_xs Config.l_host.Config.ip 
            in
            (* configure synjitsu *)
            match t.synjitsu with
            | Some s -> begin
                t.log (green "Notifying Synjitsu of MAC %s\n" (Macaddr.to_string mac));
                try_lwt 
                  Synjitsu.send_garp s mac ip 
                with e -> 
                  t.log (green "Got exception %s\n" (Printexc.to_string e)); 
                  Lwt.return_unit
              end
            | None -> Lwt.return_unit
          end
         | None -> Lwt.return_unit
       end
      | None -> t.log (Printf.sprintf "VM %s has no IP. Synjitsu not notified." vm_name); Lwt.return_unit
    in
    match vm_state with
    | Vm_state.Running -> (* Already running, exit *)
      t.log (yellow " --! VM is already running\n");
      Lwt.return_unit
    | Vm_state.Paused ->
      t.log " --> resuming vm...\n";
      or_vm_backend_error "Unable to resume VM" (Vm_backend.resume_vm t.vm_backend) vm >>= fun () ->
      Lwt.async(notify_synjitsu);
      update_stats () >>= fun () ->
      Irmin_backend.get_response_delay t.storage ~vm_name >>= fun delay ->
      Lwt_unix.sleep delay      
    | Vm_state.Off ->
      t.log " --> creating vm...\n";
      or_vm_backend_error "Unable to create VM" (Vm_backend.start_vm t.vm_backend) vm >>= fun () ->
      Lwt.async(notify_synjitsu);
      update_stats () >>= fun () ->
      Irmin_backend.get_response_delay t.storage ~vm_name >>= fun delay ->
      Lwt_unix.sleep delay
    | Vm_state.Unknown ->
      t.log (yellow " --! VM cannot be started from this state.\n");
      Lwt.return_unit

  let output_stats t vm_names =
    let current_time = Unix.time () in
    let ip_option_to_string ip_option = 
      match ip_option with
      | None -> "None"
      | Some ip -> Ipaddr.V4.to_string ip
    in
    let ts f =
      match f with
      | None -> "Never"
      | Some f -> (string_of_float ( f -. current_time )) ^ " ago"
    in
    Lwt_list.iter_s (fun vm_name ->
        or_vm_backend_error (Printf.sprintf "Unable to look up VM name: %s" vm_name) (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm ->
        t.log (Printf.sprintf "%15s %10s %10s %10s %10s %10s %30s %15s %8s %8s %8s %s\n" "VM" "state" "delay" "start_time" "tot_starts" "stop_mode" "DNS" "IP" "TTL" "tot_req" "last_req" "application");
        get_vm_state t vm >>= fun vm_state ->
        Irmin_backend.get_ip t.storage ~vm_name >>= fun vm_ip ->
        Irmin_backend.get_response_delay t.storage ~vm_name >>= fun response_delay ->
        Irmin_backend.get_start_timestamp t.storage ~vm_name >>= fun start_ts ->
        Irmin_backend.get_total_starts t.storage ~vm_name >>= fun total_starts ->
        Irmin_backend.get_stop_mode t.storage ~vm_name >>= fun stop_mode ->
        let first_part = (Printf.sprintf "%15s %10s %10f %10s %10d %10s" 
                            vm_name 
                            (Vm_state.to_string vm_state)
                            response_delay
                            (ts start_ts)
                            total_starts
                            (Vm_stop_mode.to_string stop_mode)) 
        in
        (* Get list of DNS domains for this vm_name *)
        Irmin_backend.get_vm_dns_name_list t.storage ~vm_name >>= fun dns_name_list ->
        Lwt_list.iter_s (fun dns_name ->
            Irmin_backend.get_last_request_timestamp t.storage ~vm_name ~dns_name >>= fun last_request_ts ->
            Irmin_backend.get_total_requests t.storage ~vm_name ~dns_name >>= fun total_requests ->
            Irmin_backend.get_ttl t.storage ~vm_name ~dns_name >>= fun ttl ->
            Irmin_backend.get_app_id t.storage ~vm_name >>= fun app_id ->
            t.log (first_part ^ (Printf.sprintf " %30s %15s %8d %8d %7s %d\n" 
                                   (Dns.Name.to_string dns_name)
                                   (ip_option_to_string vm_ip)
                                   ttl
                                   total_requests
                                   (ts last_request_ts)
                                   app_id));
            Lwt.return_unit
          ) dns_name_list >>= fun () ->
        if dns_name_list = [] then
          t.log "\n";
        Lwt.return_unit) vm_names

  (** Process function for ocaml-dns. Starts new VMs from DNS queries or
      forwards request to a fallback resolver *)
  let process t ~src:_ ~dst:_ packet =
    Dns_helpers.create_dns_db t.storage >>= fun dns_db ->
    let open Packet in
    match packet.questions with
    | [] -> return_none;
    | [q] -> begin
        let answer = Query.(answer q.q_name q.q_type dns_db.Loader.trie) in
        match answer.Query.rcode with
        | Packet.NoError ->
          t.log (Printf.sprintf "Local match for domain %s\n"
                   (Name.to_string q.q_name));
          (* look for vms in irmin that have the dns domain registered *)
          Irmin_backend.get_vm_list t.storage >>= fun vm_list ->
          Lwt_list.filter_map_s (fun vm_name ->
              Irmin_backend.get_vm_dns_name_list t.storage ~vm_name >>= fun dns_name_list ->
              Lwt_list.filter_map_s (fun dns_name ->
                  if dns_name = q.q_name then (* we found a match, update stats and add to list *)
                    Irmin_backend.inc_total_requests t.storage ~vm_name ~dns_name >>= fun () ->
                    Irmin_backend.set_last_request_timestamp t.storage ~vm_name ~dns_name (Unix.time()) >>= fun () ->
                    t.log (Printf.sprintf "Matching VM is %s with DNS name %s\n" vm_name (Dns.Name.to_string dns_name));
                    Lwt.return (Some dns_name)
                  else
                    Lwt.return_none
                ) dns_name_list >>= fun matching_dns_names ->
              if (List.length matching_dns_names) > 0 then
                Lwt.return (Some vm_name)
              else
                Lwt.return_none
            ) vm_list >>= fun matching_vm_names ->
          Lwt_list.filter_map_s (fun vm_name -> (* start VMs and return IPs *)
              Irmin_backend.get_ip t.storage ~vm_name >>= fun r ->
              match r with
              | None -> Lwt.return_none (* no ip, no result to return *)
              | Some ip ->
                or_vm_backend_error (Printf.sprintf "Unable to look up VM name: %s" vm_name) (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm ->
                set_timestamp t ~vm_name ~app_name:vm_name ~label:"last_su" ~time:(Unix.time ()) >>= fun () ->
                start_vm' t vm true Config.l_host.Config.ip >>= fun () ->
                output_stats t [vm_name] >>= fun () ->
                Lwt.return (Some ip)
            ) matching_vm_names >>= fun list_of_ips ->
          if (List.length list_of_ips) = 0 then begin
            t.log (Printf.sprintf "No valid match for %s. Forwarding.\n" (Dns.Name.to_string q.q_name));
            Dns_helpers.fallback t.forward_resolver q.q_class q.q_type q.q_name
          end else 
            (* TODO how to return results with multiple IPs - for now just return DNS answer *)
            return (Some answer)
        | _ ->
          t.log (Printf.sprintf "No local match for %s, forwarding...\n"
                   (Name.to_string q.q_name));
          Dns_helpers.fallback t.forward_resolver q.q_class q.q_type q.q_name
      end
    | _ -> return_none

  (* add vm to be monitored by jitsu *)
  let add_vm t ~vm_name ~vm_ip ~vm_stop_mode ~dns_names ~dns_ttl ~response_delay =
    (* check if vm_name exists and set up VM record *)

    or_vm_backend_error "Unable to lookup VM by name" (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm_dom ->
    or_vm_backend_error "Unable to get MAC for VM" (Vm_backend.get_mac t.vm_backend) vm_dom >>= fun vm_mac ->

    Irmin_backend.add_vm t.storage ~vm_name ~vm_mac ~vm_ip ~vm_stop_mode ~response_delay >>= fun () ->
    let app_id = int_of_string (Str.global_replace (Str.regexp "\.+") "" (Ipaddr.V4.to_string vm_ip)) in
    Irmin_backend.set_app_id t.storage ~vm_name app_id >>= fun () ->
    Lwt_list.iter_s (fun dns_name ->
        Irmin_backend.add_vm_dns t.storage ~vm_name ~dns_name ~dns_ttl
      ) dns_names

  let get_remote_host () =
    (* TODO: replace with selection algorithm *)
    List.nth Config.r_hosts 0

  let add_replica t ~vm_name ~params =
    t.log "add_replica function";
    let app_name = Rpc.string_of_rpc (List.nth params 0) in
    t.log "app_name extracted";
    lwt safe_time =
      lwt time = get_timestamp t ~vm_name ~app_name ~label:"last_su" in
      return ((Unix.time () -. time) > Config.ignore_time)
    in
    if safe_time then
      let _ = t.log "safe_time..." in
      let is_host vm_name =
        List.exists (fun h -> h.Config.name = vm_name) Config.r_hosts
      in
      let on_remote () = 
        let _ = first_replica := not (!first_replica) in !first_replica
      in (* TODO: replace with decision algorithm *)
      let local_build (app_id, stop_mode, ttl, kernel) initial_xs =
        t.log "local build function";
        let (rep_mac, rep_ip) = List.hd !Config.mac_ip in Config.mac_ip := List.tl !Config.mac_ip;
        let rep_name = Macaddr.to_string rep_mac in
        
        or_vm_backend_error "Unable to define replica"
          (Vm_backend.define_vm ~name_label:rep_name ~mAC:rep_mac ~pV_kernel:kernel) t.vm_backend >>= fun _ ->
        
        add_vm t ~vm_name:rep_name ~vm_ip:rep_ip ~vm_stop_mode:stop_mode
          ~dns_names:[Dns.Name.of_string "replica"] ~dns_ttl:ttl ~response_delay:0.0 >>= fun () ->
        
        Irmin_backend.set_last_request_timestamp t.storage ~vm_name:rep_name
          ~dns_name:(Dns.Name.of_string "replica") (Unix.time()) >>= fun () ->
        
        Irmin_backend.set_app_id t.storage ~vm_name:rep_name app_id >>= fun () ->
        
        or_vm_backend_error (Printf.sprintf "Unable to look up VM name: %s" rep_name)
          (Vm_backend.lookup_vm_by_name t.vm_backend) rep_name >>= fun vm ->
        
        Irmin_backend.set_initial_xs t.storage ~vm_name:rep_name initial_xs.Config.name >>= fun () ->  
        
        start_vm' t vm false initial_xs.Config.ip >>= fun () ->
        set_timestamp t ~vm_name:rep_name ~app_name ~label:"last_su" ~time:(Unix.time ()) >>= fun () ->
        get_num_of_reps t ~vm_name:rep_name ~app_name >>= fun num ->
        set_num_of_reps t ~vm_name:rep_name ~app_name ~number:(num + 1)
      in
      let remote_build (app_id, stop_mode, ttl, kernel) remote_host =
        Irmin_backend.create ~address:remote_host.Config.irmin_store () >>= fun storage ->
        Irmin_backend.add_replica storage ~vm_name:Config.l_host.Config.name ~app_name 
          ~app_id:app_id ~vm_stop_mode:stop_mode ~ttl ~kernel  
      in
      let extract_params_from_vm vm vm_name =
        lwt kernel = or_vm_backend_error
          "Unable to get VM kernel" (Vm_backend.get_kernel t.vm_backend) vm  in
        lwt app_id    = Irmin_backend.get_app_id t.storage ~vm_name in
        lwt ttl       = Irmin_backend.get_ttl t.storage ~vm_name ~dns_name:(Dns.Name.of_string "replica") in
        lwt stop_mode = Irmin_backend.get_stop_mode t.storage ~vm_name in
        return ( app_id, stop_mode, ttl, kernel )
      in
      let extract_params_from_irmin () =
        t.log (Printf.sprintf "extract params from irmin\n size of params: %d\n\n" (List.length params));
        (* TODO: return error when number of params is not right *)
        let app_id    = Rpc.int_of_rpc (List.nth params 1) in
        let ttl       = Rpc.int_of_rpc (List.nth params 2) in
        let stop_mode = Vm_stop_mode.of_string (Rpc.string_of_rpc (List.nth params 3)) in
        let kernel    = Uri.of_string (Rpc.string_of_rpc (List.nth params 4)) in
        return ( app_id, stop_mode, ttl, kernel )
      in
      t.log "lookup name";
      lwt (params, initial_xs, remote) =
        Vm_backend.lookup_vm_by_name t.vm_backend vm_name >>= function
        | `Error e -> begin
          match e with (* TODO *)
          | _ -> begin (* request from original vm/remote vm *)
             match is_host vm_name with
             | true -> (* remote host requested a vm *)
               t.log (Printf.sprintf "request from another Jitsu: %s" vm_name);
               extract_params_from_irmin () >>= fun p ->
               let i_xs = List.find (fun h -> h.Config.name = vm_name) Config.r_hosts in
               return (p, i_xs, false)
             | false ->
               Irmin_backend.get_mac t.storage ~vm_name:app_name >>= fun mac ->
               match mac with
               | None -> (* delete entry instead of raising an exception? *)
                         raise (Failure (red "Wrong application name or MAC address is not set"))
               | Some m ->
                 t.log "Request by the original VM";
                 if (Macaddr.to_string m) = vm_name then
                    or_vm_backend_error "Unable to lookup VM by name"
                      (Vm_backend.lookup_vm_by_name t.vm_backend) app_name >>= fun vm ->
                    extract_params_from_vm vm app_name >>= fun p ->
                    return (p, Config.l_host, on_remote ())
                 else (* security risk *)
                   raise (Failure (red "Request from unknown entity!"))
          end            
        (*| `Disconnected -> raise (Failure (red "Unable to lookup VM by name: Disconnected"))
          | `Unknown s -> raise (Failure (red "Unable to lookup VM by name: %s" s))
        *)
        end
        | `Ok r -> (* request from a local vm but not the original *)
          extract_params_from_vm r vm_name >>= fun p ->
          Irmin_backend.get_initial_xs t.storage ~vm_name >>= fun xs ->
          match xs with
          | x when x = Config.l_host.Config.name -> return (p, Config.l_host, on_remote ()) 
          | _ ->
             let i_xs = List.find (fun h -> h.Config.name = xs) Config.r_hosts in
             return (p, i_xs, false)
      in 
        if remote then
          remote_build params (get_remote_host ())
        else
          local_build params initial_xs
    else
      let _ = t.log "Not the right time to replicate" in 
      return ()
       
  let del_replica t ~vm_name ~params = (* name is a mac address *)
    let app_name = Rpc.string_of_rpc (List.nth params 0) in
    lwt safe_time =
      lwt time = get_timestamp t ~vm_name ~app_name ~label:"last_sd" in
      return ((Unix.time () -. time) > Config.ignore_time)
    in
    if safe_time then
      let _ = t.log "deleting vm...." in
      Irmin_backend.set_ttl t.storage ~vm_name:vm_name ~dns_name:(Dns.Name.of_string "replica") 0 >>= fun () ->
      set_timestamp t ~vm_name ~app_name ~label:"last_su" ~time:(Unix.time ()) >>= fun () ->
      get_num_of_reps t ~vm_name ~app_name >>= fun num ->
      set_num_of_reps t ~vm_name ~app_name ~number:(num - 1)
    else
      return ()

  (* iterate through t.name_table and stop VMs that haven't received
     requests for more than ttl*2 seconds *)
    let stop_expired_vms t =
    Irmin_backend.get_vm_list t.storage >>= fun vm_name_list ->
    (* Check for expired names *)
    Lwt_list.filter_map_s (fun vm_name ->
        or_vm_backend_error (Printf.sprintf "Unable to look up VM name: %s" vm_name) (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm ->
        get_vm_state t vm >>= fun vm_state ->
        match vm_state with
        | Vm_state.Off 
        | Vm_state.Paused 
        | Vm_state.Unknown -> Lwt.return_none (* VM already stopped/paused/crashed.. *)
        | Vm_state.Running ->
          (* Get list of DNS domains that have been requested (has requested timestamp != None) and has NOT expired (timestamp is younger than ttl*2) *)
          Irmin_backend.get_vm_dns_name_list t.storage ~vm_name >>= fun dns_name_list ->
          Lwt_list.filter_map_s (fun dns_name ->
              Irmin_backend.get_last_request_timestamp t.storage ~vm_name ~dns_name >>= fun r ->
              match r with
              | None -> Lwt.return_none (* name not requested, can't expire *)
              | Some last_request_ts ->
                let current_time = Unix.time () in
                Irmin_backend.get_ttl t.storage ~vm_name ~dns_name >>= fun ttl ->
                if ((ttl < 0) || (current_time -. last_request_ts <= (float_of_int (ttl * 2)))) then
                  Lwt.return (Some dns_name)
                else
                  Lwt.return_none
            ) dns_name_list
          >>= fun unexpired_dns_names ->
          if (List.length unexpired_dns_names) > 0 then (* If VM has unexpired DNS domains, DON'T terminate *)
            Lwt.return_none
          else
            Lwt.return (Some vm) (* VM has no unexpired DNS domains, can be terminated *)
      ) vm_name_list >>= fun expired_vms ->
    Lwt_list.iter_s (stop_vm t) expired_vms (* Stop expired VMs *)
    
end