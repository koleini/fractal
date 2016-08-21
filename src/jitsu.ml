(*
 * Copyright (c) 2014-2015 Magnus Skjegstad <magnus@skjegstad.com>
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
open Dns

let red fmt    = Printf.sprintf ("\027[31m"^^fmt^^"\027[m")
let yellow fmt = Printf.sprintf ("\027[33m"^^fmt^^"\027[m")

type scale_mode = Add | Delete | Disable

module Make (Vm_backend : Backends.VM_BACKEND) (Storage_backend : Backends.STORAGE_BACKEND) = struct
  module Synjitsu = Synjitsu.Make(Vm_backend)
  module DC = Datacenter.Make(Storage_backend)

  type t = {
    mutable dns_db : Loader.db;                         (* DNS database *)
    storage : Storage_backend.t;
    log : (string -> unit);                   (* Log function *)
    vm_backend : Vm_backend.t;                          (* Backend type *)
    forward_resolver : Dns_resolver_unix.t option; (* DNS to forward request to if no
                                                      local match *)
    synjitsu : Synjitsu.t option;
    mutable datacenter : DC.t;
  }

  let get_storage t = t.storage

  let get_datacenter t = t.datacenter

  let update_datacenter t servers = t.datacenter <- servers

  let create vm_backend log forward_resolver ?synjitsu:(synjitsu=None) ?persistdb:(root=None) datacenter () =
    (* initialise synjitsu *)
    let synjitsu_logger s = log (Printf.sprintf "synjitsu: %s" s) in
    let synjitsu = match synjitsu with
      | Some domain -> let t = (Synjitsu.create vm_backend synjitsu_logger domain "synjitsu") in
        ignore_result (Synjitsu.connect t);  (* connect in background *)
        Some t
      | None -> None
    in
    let irmin_logger s = log (Printf.sprintf "irmin_backend: %s" s) in
    (match root with
      | Some root -> Storage_backend.create ~root ~log:irmin_logger ~address:(DC.get_irmin_uri ()) ()
      | None -> Storage_backend.create ~log:irmin_logger ~address:(DC.get_irmin_uri ()) ()) >>= fun storage ->
    Lwt.return {
      dns_db = Loader.new_db ();
      storage;
      log;
      vm_backend;
      forward_resolver = forward_resolver;
      synjitsu ;
      datacenter;
    }

  let string_of_error e =
    match e with
    | `Invalid_config s -> (Printf.sprintf "Invalid config: %s" s)
    | `Not_found -> "Not found"
    | `Not_supported -> "Not supported"
    | `Disconnected s -> (Printf.sprintf "Disconnected: %s" s)
    | `Unable_to_connect s -> (Printf.sprintf "Unable to connect: %s" s)
    | `Unknown s -> (Printf.sprintf "%s" s)

  let or_vm_backend_error msg fn t =
    fn t >>= function
    | `Error e -> raise (Failure (Printf.sprintf "%s: %s" (string_of_error e) msg))
    | `Ok t -> return t

  let get_vm_name t vm_uuid =
    or_vm_backend_error "Unable to get VM name from backend" (Vm_backend.get_name t.vm_backend) vm_uuid >>= fun vm_name ->
    match vm_name with
    | None -> Lwt.return "<unknown>"
    | Some s -> Lwt.return s

  let lwt_the msg f =
    f >>= function
    | None -> raise (Failure msg)
    | Some r -> return r

  let get_vm_state t vm_uuid =
    or_vm_backend_error "Unable to get VM state from backend" (Vm_backend.get_state t.vm_backend) vm_uuid

  let config_ovs t requestor mode ?vm_uuid ?domid () = (* TODO: combine with notify_synjitsu () *)
    lwt vm_uuid =
      match vm_uuid with
      | None ->
        let msg = Printf.sprintf "No UUID match for the VM %s" requestor in
        lwt_the msg (Storage_backend.get_uuid_of_mac t.storage ~vm_mac:(Macaddr.of_string_exn requestor))
      | Some u -> Lwt.return u
    in
    Storage_backend.get_ip t.storage ~vm_uuid >>= fun r ->
    or_vm_backend_error "Unable to get MAC for VM" (Vm_backend.get_mac t.vm_backend) vm_uuid >>= fun vm_mac ->
    match r, vm_mac with
    | Some ip, m::_ -> begin
      lwt dom_id =
        match domid with
        | None -> or_vm_backend_error "Unable to get domain id for VM" (Vm_backend.get_domain_id t.vm_backend) vm_uuid
        | Some d -> return d
      in
      Storage_backend.get_appid t.storage ~vm_uuid >>= function
      | 0 -> raise (Failure (red "%s: application number unknown" (Uuidm.to_string vm_uuid)))
      | appid ->
        Storage_backend.get_init_host t.storage ~vm_uuid >>= fun init_host ->
        Storage_backend.get_init_uuid t.storage ~vm_uuid >>= fun init_uuid ->
        lwt _ =
        match init_uuid, mode with
        | Some _, Add -> Ovs_backend.add_replica appid ip m dom_id (DC.find_host_by_name t.datacenter init_host).DC.ip (DC.get_local_host t.datacenter).DC.ip
        | Some _, Delete -> Ovs_backend.del_replica appid ip m dom_id (DC.find_host_by_name t.datacenter init_host).DC.ip (DC.get_local_host t.datacenter).DC.ip
        | Some _, Disable -> Ovs_backend.dis_replica appid ip m dom_id (DC.find_host_by_name t.datacenter init_host).DC.ip (DC.get_local_host t.datacenter).DC.ip
        | None, Add -> Ovs_backend.add_application appid ip m dom_id (DC.get_local_host t.datacenter).DC.ip
        | None, Delete -> Ovs_backend.del_application appid ip m dom_id (DC.get_local_host t.datacenter).DC.ip
        | None, Disable -> Ovs_backend.dis_replica appid ip m dom_id (DC.find_host_by_name t.datacenter init_host).DC.ip (DC.get_local_host t.datacenter).DC.ip
        (*
        | Some _, Add -> NaasLoad.add_replica_of13 (DC.find_host_by_name t.datacenter init_host).DC.controller (DC.get_local_host t.datacenter).DC.controller (Int32.of_int appid) ip m dom_id
        | Some _, Delete -> NaasLoad.delete_replica_of13 (DC.find_host_by_name t.datacenter init_host).DC.controller (DC.get_local_host t.datacenter).DC.controller (Int32.of_int appid) ip dom_id
        | Some _, Disable -> NaasLoad.disable_replica_of13 (DC.find_host_by_name t.datacenter init_host).DC.controller (DC.get_local_host t.datacenter).DC.controller (Int32.of_int appid) ip dom_id
        | None, Add -> NaasLoad.add_app_of13 (DC.get_local_host t.datacenter).DC.controller (Int32.of_int appid) ip m dom_id ip
        | _, _ -> raise (Failure (red "OVS configuration not supported."))
        *)
        in
          return m
    end
    | _, [] -> raise (Failure (red "VM %s has no MAC address. OVS not updated." (Uuidm.to_string vm_uuid)))
    | None, _ -> raise (Failure (red "VM %s has no IP. OVS not updated." (Uuidm.to_string vm_uuid)))

  let stop_vm t vm_uuid =
    get_vm_state t vm_uuid >>= fun vm_state ->
    match vm_state with
    | Vm_state.Running ->
      get_vm_name t vm_uuid >>= fun vm_name ->
      let uuid_s = Uuidm.to_string vm_uuid in
      Storage_backend.get_stop_mode t.storage ~vm_uuid >>= fun stop_mode ->
      config_ovs t "00:00:00:00:00:00" Delete ~vm_uuid () >>= fun vm_mac ->
      begin match stop_mode with
        | Vm_stop_mode.Unknown -> t.log (Printf.sprintf "Unable to stop VM %s (%s). Unknown stop mode requested." uuid_s vm_name);
          Lwt.return_unit
        | Vm_stop_mode.Shutdown -> t.log (Printf.sprintf "VM shutdown: %s (%s)" uuid_s vm_name);
          or_vm_backend_error "Unable to shutdown VM" (Vm_backend.shutdown_vm t.vm_backend) vm_uuid >>= fun () -> begin
          Storage_backend.get_init_uuid t.storage ~vm_uuid >>= function
          | None -> Lwt.return_unit
          | Some _ -> or_vm_backend_error "Unable to undefine VM" (Vm_backend.undefine_vm t.vm_backend) vm_uuid >>= fun () ->
            Storage_backend.delete_vm t.storage ~vm_uuid ~vm_mac
        end
        | Vm_stop_mode.Suspend  -> t.log (Printf.sprintf "VM suspend: %s (%s)" uuid_s vm_name);
          or_vm_backend_error "Unable to suspend VM" (Vm_backend.suspend_vm t.vm_backend) vm_uuid
        | Vm_stop_mode.Destroy  -> t.log (Printf.sprintf "VM destroy: %s (%s)" uuid_s vm_name) ;
          or_vm_backend_error "Unable to destroy VM" (Vm_backend.destroy_vm t.vm_backend) vm_uuid
      end
    | Vm_state.Off
    | Vm_state.Paused
    | Vm_state.Suspended
    | Vm_state.Unknown -> Lwt.return_unit (* VM already stopped or nothing we can do... *)

  let start_vm t vm_uuid =
    get_vm_state t vm_uuid >>= fun vm_state ->
    get_vm_name t vm_uuid >>= fun vm_name ->
    t.log (Printf.sprintf "Starting VM %s (name=%s, state=%s)" (Uuidm.to_string vm_uuid) vm_name (Vm_state.to_string vm_state));
    let update_stats vm_mac () =
      Storage_backend.set_start_timestamp t.storage ~vm_uuid (Unix.time ()) >>= fun () ->
      Storage_backend.inc_total_starts t.storage ~vm_uuid >>= fun () ->
      Storage_backend.set_uuid_for_mac t.storage ~vm_mac ~vm_uuid
    in
    Storage_backend.get_use_synjitsu t.storage ~vm_uuid >>= fun use_synjitsu ->
    let notify_synjitsu () =
      match use_synjitsu, t.synjitsu with
      | _, None -> Lwt.return_unit (* synjitsu not configured *)
      | false, Some _ -> Lwt.return_unit (* synjitsu disabled for domain *)
      | true, Some s -> begin
          Storage_backend.get_ip t.storage ~vm_uuid >>= fun r ->
          or_vm_backend_error "Unable to get MAC for VM" (Vm_backend.get_mac t.vm_backend) vm_uuid >>= fun vm_mac ->
          match r, vm_mac with
          | Some ip, [m] -> begin
              t.log (Printf.sprintf "Notifying Synjitsu of MAC %s" (Macaddr.to_string m));
              try_lwt
                Synjitsu.send_garp s m ip
              with e ->
                t.log (Printf.sprintf "Got exception %s" (Printexc.to_string e));
                Lwt.return_unit
            end
          | _, [] -> t.log (Printf.sprintf "VM %s has no MAC address. Synjitsu not notified." (Uuidm.to_string vm_uuid)); Lwt.return_unit
          | _, _::_::_ -> t.log (Printf.sprintf "VM %s has multiple MAC addresses (not supported). Synjitsu not notified." (Uuidm.to_string vm_uuid)); Lwt.return_unit
          | None, _ -> t.log (Printf.sprintf "VM %s has no IP. Synjitsu not notified." (Uuidm.to_string vm_uuid)); Lwt.return_unit
        end
    in
    let wait () =
      let wait_for_key =
        Storage_backend.get_wait_for_key t.storage ~vm_uuid >>= fun wait_key ->
        match wait_key with
        | Some k -> begin
            try
              or_vm_backend_error "Unable to get dom ID of VM" (Vm_backend.get_domain_id t.vm_backend) vm_uuid >>= fun domid ->
              Xenstore.wait_by_domid domid k ~timeout:2.0 () >>= function
              | `Ok  -> t.log (Printf.sprintf "Key %s appeared in Xenstore - domain %s (domid=%d) ready." k (Uuidm.to_string vm_uuid) domid) ; Lwt.return_unit
              | `Timeout -> t.log (Printf.sprintf "Timed out while waiting for key %s to appear in Xenstore. Sending DNS reply anyway." k); Lwt.return_unit
            with
            | Failure msg -> t.log (Printf.sprintf "Unable to get domain ID and wait for key to appear in Xenstore. Sending DNS reply anyway. Error:\n%s" msg); Lwt.return_unit
          end
        | None -> Lwt.return_unit
      in
      let sleep =
        Storage_backend.get_response_delay t.storage ~vm_uuid >>= fun delay ->
        Lwt_unix.sleep delay
      in
      wait_for_key >>= fun () ->
      sleep
    in
    match vm_state with
    | Vm_state.Running -> (* Already running, exit *)
      t.log " --! VM is already running";
      Lwt.return_unit
    | Vm_state.Suspended ->
      t.log " --> resuming VM...";
      or_vm_backend_error "Unable to resume VM" (Vm_backend.resume_vm t.vm_backend) vm_uuid >>= fun () ->
      or_vm_backend_error "Unable to get domain id for VM" (Vm_backend.get_domain_id t.vm_backend) vm_uuid >>= fun domid ->
      Storage_backend.set_domid t.storage ~vm_uuid ~domid >>= fun () ->
      config_ovs t "00:00:00:00:00:00" Add ~vm_uuid () >>= fun vm_mac ->
      Lwt.async(notify_synjitsu);
      update_stats vm_mac () >>= fun () ->
      wait ()
    | Vm_state.Paused ->
      t.log " --> unpausing VM...";
      or_vm_backend_error "Unable to unpause VM" (Vm_backend.unpause_vm t.vm_backend) vm_uuid >>= fun () ->
      or_vm_backend_error "Unable to get domain id for VM" (Vm_backend.get_domain_id t.vm_backend) vm_uuid >>= fun domid ->
      Storage_backend.set_domid t.storage ~vm_uuid ~domid >>= fun () ->
      config_ovs t "00:00:00:00:00:00" Add ~vm_uuid () >>= fun vm_mac ->
      Lwt.async(notify_synjitsu);
      update_stats vm_mac () >>= fun () ->
      wait ()
    | Vm_state.Off ->
      t.log " --> creating VM...";
      Storage_backend.get_vm_config t.storage ~vm_uuid >>= fun config ->
      or_vm_backend_error "Unable to create VM" (Vm_backend.start_vm t.vm_backend vm_uuid) config >>= fun () ->
      or_vm_backend_error "Unable to get domain id for VM" (Vm_backend.get_domain_id t.vm_backend) vm_uuid >>= fun domid ->
      Storage_backend.set_domid t.storage ~vm_uuid ~domid >>= fun () ->
      config_ovs t "00:00:00:00:00:00" Add ~vm_uuid () >>= fun vm_mac ->
      Lwt.async(notify_synjitsu);
      update_stats vm_mac () >>= fun () ->
      wait ()
    | Vm_state.Unknown ->
      t.log " --! VM cannot be started from this state.";
      Lwt.return_unit

  let output_stats t ?vm_uuids:(vm_uuids=None) () =
    (match vm_uuids with
     | None -> Storage_backend.get_vm_list t.storage
     | Some l -> Lwt.return l) >>= fun vm_uuids ->
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
    let fmt = format_of_string "%40s %15s %10s %10s %8s %10s %10s %10s %30s %15s %8s %8s %8s" in
    (* print titles *)
    t.log (Printf.sprintf fmt "uuid" "name" "state" "delay" "synj." "start_time" "tot_starts" "stop_mode" "DNS" "IP" "TTL" "tot_req" "last_req");
    Lwt_list.iter_s (fun vm_uuid ->
        get_vm_state t vm_uuid >>= fun vm_state ->
        get_vm_name t vm_uuid >>= fun vm_name ->
        Storage_backend.get_ip t.storage ~vm_uuid >>= fun vm_ip ->
        Storage_backend.get_response_delay t.storage ~vm_uuid >>= fun response_delay ->
        Storage_backend.get_wait_for_key t.storage ~vm_uuid >>= fun wait_for_key ->
        let response_delay_str =
          match wait_for_key with
          | None -> (string_of_float response_delay)
          | Some _ -> (Printf.sprintf "wait+%s" (string_of_float response_delay))
        in
        Storage_backend.get_start_timestamp t.storage ~vm_uuid >>= fun start_ts ->
        Storage_backend.get_total_starts t.storage ~vm_uuid >>= fun total_starts ->
        Storage_backend.get_stop_mode t.storage ~vm_uuid >>= fun stop_mode ->
        (* Get list of DNS domains for this vm_name *)
        Storage_backend.get_vm_dns_name_list t.storage ~vm_uuid >>= fun dns_name_list ->
        Storage_backend.get_use_synjitsu t.storage ~vm_uuid >>= fun synjitsu ->
        Lwt_list.iter_s (fun dns_name ->
            Storage_backend.get_last_request_timestamp t.storage ~vm_uuid ~dns_name >>= fun last_request_ts ->
            Storage_backend.get_total_requests t.storage ~vm_uuid ~dns_name >>= fun total_requests ->
            Storage_backend.get_ttl t.storage ~vm_uuid ~dns_name >>= fun ttl ->
            t.log (Printf.sprintf fmt
                     (Uuidm.to_string vm_uuid)
                     vm_name
                     (Vm_state.to_string vm_state)
                     (response_delay_str)
                     (string_of_bool synjitsu)
                     (ts start_ts)
                     (string_of_int total_starts)
                     (Vm_stop_mode.to_string stop_mode)
                     (Dns.Name.to_string dns_name)
                     (ip_option_to_string vm_ip)
                     (string_of_int ttl)
                     (string_of_int total_requests)
                     (ts last_request_ts));
            Lwt.return_unit
          ) dns_name_list >>= fun () ->
        if dns_name_list = [] then (* no DNS entries, output first part only *)
          t.log (Printf.sprintf fmt
                   (Uuidm.to_string vm_uuid)
                   vm_name
                   (Vm_state.to_string vm_state)
                   (response_delay_str)
                   (string_of_bool synjitsu)
                   (ts start_ts)
                   (string_of_int total_starts)
                   (Vm_stop_mode.to_string stop_mode)
                   "(none)" "" "" "" "");
        Lwt.return_unit) vm_uuids

  (* add vm to be monitored by jitsu *)
  let add_vm t ~vm_ip ~vm_stop_mode ~dns_names ~dns_ttl ~response_delay ~wait_for_key ~use_synjitsu ~vm_config =
    or_vm_backend_error "Unable to configure VM" (Vm_backend.configure_vm t.vm_backend) vm_config >>= fun vm_uuid ->
    let appid = int_of_string (Str.global_replace (Str.regexp "\.+") "" (Ipaddr.V4.to_string vm_ip)) in (* BUG: only has to be set for the original one *)
    Storage_backend.add_vm t.storage ~vm_uuid ~vm_ip ~vm_stop_mode ~response_delay ~wait_for_key ~use_synjitsu ~vm_config >>= fun () -> (* BUG *)
    Storage_backend.set_appid t.storage ~vm_uuid ~appid >>= fun () ->  (* BUG *)
    Lwt_list.iter_s (fun dns_name ->
        Storage_backend.add_vm_dns t.storage ~vm_uuid ~dns_name ~dns_ttl
      ) dns_names

  (* check if VM is rebooted *)
  let vm_reboot_check t vm_uuid =
    lwt domid = or_vm_backend_error "Unable to get domain id for VM" (Vm_backend.get_domain_id t.vm_backend) vm_uuid in
    lwt domid' = Storage_backend.get_domid t.storage ~vm_uuid in
    if domid != domid' then
      (* remove previous OvS flows *)
      config_ovs t "00:00:00:00:00:00" Delete ~vm_uuid ~domid:domid' () >>= fun _ ->
      (* add new OvS flows *)
      Storage_backend.set_domid t.storage ~vm_uuid ~domid >>= fun () ->
      config_ovs t "00:00:00:00:00:00" Add ~vm_uuid ~domid () >>= fun _ ->
      Lwt.return_unit
    else
      Lwt.return_unit

  (* iterate through t.name_table and stop VMs that haven't received
     requests for more than ttl*2 seconds *)
  let stop_expired_vms t =
    Storage_backend.get_vm_list t.storage >>= fun vm_uuid_list ->
    (* Check for expired names *)
    Lwt_list.filter_map_s (fun vm_uuid ->
        get_vm_state t vm_uuid >>= fun vm_state ->
        match vm_state with
        | Vm_state.Off
        | Vm_state.Paused
        | Vm_state.Suspended
        | Vm_state.Unknown ->
          (* if the status of VM in Irmin is running, then OVS config has to get fixed *)
          lwt irmin_state = Storage_backend.get_vm_state t.storage ~vm_uuid in begin
          match irmin_state with
          | Vm_state.Running ->
            t.log (Printf.sprintf "VM %s is crashed" (Uuidm.to_string vm_uuid));
            config_ovs t "00:00:00:00:00:00" Delete ~vm_uuid () >>= fun _ ->
            lwt () = Storage_backend.set_vm_state t.storage ~vm_uuid Vm_state.Off in (* TODO: undefine *)
            Lwt.return_none
          | _ -> Lwt.return_none
          end
        | Vm_state.Running ->
          (* Get list of DNS domains that have been requested (has requested timestamp != None) and has NOT expired (timestamp is younger than ttl*2) *)
          vm_reboot_check t vm_uuid >>= fun () ->
          Storage_backend.get_vm_dns_name_list t.storage ~vm_uuid >>= fun dns_name_list ->
          Lwt_list.filter_map_s (fun dns_name ->
              Storage_backend.get_last_request_timestamp t.storage ~vm_uuid ~dns_name >>= fun r ->
              lwt time_stamp =
                match r with
                | None -> lwt_the "start timestamp empty" (Storage_backend.get_start_timestamp t.storage ~vm_uuid)
                | Some last_request_ts -> return last_request_ts
              in
                let current_time = Unix.time () in
                Storage_backend.get_ttl t.storage ~vm_uuid ~dns_name >>= fun ttl ->
                if ((ttl < 0) || (current_time -. time_stamp <= (float_of_int (ttl * 2)))) then
                  Lwt.return (Some dns_name)
                else
                  Lwt.return_none
            ) dns_name_list
          >>= fun unexpired_dns_names ->
          if (List.length unexpired_dns_names) > 0 then (* If VM has unexpired DNS domains, DON'T terminate *)
            Lwt.return_none
          else
            Lwt.return (Some vm_uuid) (* VM has no unexpired DNS domains, can be terminated *)
      ) vm_uuid_list >>= fun expired_vms ->
    Lwt_list.iter_s (stop_vm t) expired_vms (* Stop expired VMs *)

  (** Process function for ocaml-dns. Starts new VMs from DNS queries or
      forwards request to a fallback resolver *)
  let process t ~src ~dst packet =
    let (src,sport) = src in
    let (dst,dport) = dst in
    t.log (Printf.sprintf "dns: Query from %s:%d to %s:%d" (Ipaddr.to_string src) sport (Ipaddr.to_string dst) dport);
    Storage_backend.get_dns_db t.storage >>= fun dns_db ->
    let open Packet in
    match packet.questions with
    | [] -> return_none;
    | [q] -> begin
        let answer = Query.(answer q.q_name q.q_type dns_db.Loader.trie) in
        match answer.Query.rcode with
        | Packet.NoError ->
          let q_name_str = Name.to_string q.q_name in
          let q_name_str_lower = String.lowercase q_name_str in
          t.log (Printf.sprintf "dns: local match for domain %s" q_name_str);
          (* look for vms in irmin that have the dns domain registered *)
          Storage_backend.get_vm_list t.storage >>= fun vm_list ->
          Lwt_list.filter_map_s (fun vm_uuid ->
              Storage_backend.get_vm_dns_name_list t.storage ~vm_uuid >>= fun dns_name_list ->
              Lwt_list.filter_map_s (fun dns_name ->
                  if ((String.lowercase (Name.to_string dns_name)) = q_name_str_lower) then (* we found a match, update stats and add to list *)
                    Storage_backend.inc_total_requests t.storage ~vm_uuid ~dns_name >>= fun () ->
                    Storage_backend.set_last_request_timestamp t.storage ~vm_uuid ~dns_name (Unix.time()) >>= fun () ->
                    t.log (Printf.sprintf "dns: matching VM is %s (dns=%s)" (Uuidm.to_string vm_uuid) (Dns.Name.to_string dns_name));
                    Lwt.return (Some dns_name)
                  else
                    Lwt.return_none
                ) dns_name_list >>= fun matching_dns_names ->
              if (List.length matching_dns_names) > 0 then
                Lwt.return (Some vm_uuid)
              else
                Lwt.return_none
            ) vm_list >>= fun matching_vm_uuids ->
          Lwt_list.filter_map_s (fun vm_uuid -> (* start VMs and return IPs *)
              Storage_backend.get_ip t.storage ~vm_uuid >>= fun r ->
              match r with
              | None -> Lwt.return_none (* no ip, no result to return *)
              | Some ip ->
                start_vm t vm_uuid >>= fun () ->
                Lwt.async ( fun () -> output_stats t ~vm_uuids:(Some [vm_uuid]) ());
                Lwt.return (Some ip)
            ) matching_vm_uuids >>= fun list_of_ips ->
          if (List.length list_of_ips) = 0 then begin
            t.log (Printf.sprintf "dns: no valid match for %s, forwarding..." (Dns.Name.to_string q.q_name));
            Dns_helpers.fallback t.forward_resolver q.q_class q.q_type q.q_name
          end else
            (* TODO how to return results with multiple IPs - for now just return DNS answer *)
            Lwt.return (Some answer)
        | _ ->
          t.log (Printf.sprintf "dns: no local match for %s, forwarding..." (Name.to_string q.q_name));
          Dns_helpers.fallback t.forward_resolver q.q_class q.q_type q.q_name
      end
    | _ -> t.log "dns: can't handle internal result, no response sent"; Lwt.return_none

  let get_timestamp t ~host ~vm_uuid ~label =
    let storage = (DC.find_host_by_name t.datacenter host).DC.irmin_store in
    Storage_backend.get_timestamp storage ~vm_uuid ~label

  let set_timestamp t ~host ~vm_uuid ~label =
    let storage = (DC.find_host_by_name t.datacenter host).DC.irmin_store in
    Storage_backend.set_timestamp storage ~vm_uuid ~label ~time:(Unix.time ())

  let modify_nor t ~host ~vm_uuid ~value =
    let storage = (DC.find_host_by_name t.datacenter host).DC.irmin_store in
    Storage_backend.get_nor storage ~vm_uuid >>= fun n ->
    Storage_backend.set_nor storage ~vm_uuid ~num:(n + value)

(*
 *     <local VM request> --> <resource available> -->(yes)-> local VM create
 *              |                                  |           (using requestor VM's params)
 *              |                                  |->(No)--> remote VM create
 * add_replica -|
 *              |
 *   <remote host request> -> <resource available> -->(yes)-> local VM create
 *                                                 |           (using received params)
 *                                                 |->(No)--> remote VM create
*)
  let add_replica t ~requestor ~params =
    let local_replicate (init_uuid, init_host (* type? *), _, vm_stop_mode, kernel) = (* TODO: appid check *)
      let (r_mac, r_ip) = List.hd !Dc_params.mac_ip in Dc_params.mac_ip := List.tl !Dc_params.mac_ip;
      let r_name = Macaddr.to_string r_mac in (* use mac address as the unique name for replica *)
      or_vm_backend_error "Unable to define replica VM"
        (Vm_backend.define_vm ~name_label:r_name ~mAC:r_mac ~pV_kernel:kernel) t.vm_backend >>= fun _domid -> (* M: no use? *)
      let vm_config = Hashtbl.create 3 in
      let _ = Hashtbl.add vm_config "name" r_name in
      let _ = Hashtbl.add vm_config "dns" "replica" in
      let _ = Hashtbl.add vm_config "ip" (Ipaddr.V4.to_string r_ip) in
      lwt () = add_vm t ~vm_ip:r_ip ~vm_stop_mode ~dns_names:[Dns.Name.of_string "replica"] ~dns_ttl:(-1) ~response_delay:0.0
                    ~wait_for_key:None ~use_synjitsu:false ~vm_config in (* or return uuid? *)
      or_vm_backend_error "Unable to lookup VM by name" (* remove it add_vm returns uuid *)
        (Vm_backend.lookup_vm_by_name t.vm_backend) r_name >>= fun vm_uuid ->
      Storage_backend.set_init_host t.storage ~vm_uuid init_host >>= fun () ->
      Storage_backend.set_init_uuid t.storage ~vm_uuid init_uuid >>= fun () ->
      start_vm t vm_uuid >>= fun () ->
      set_timestamp t ~host:init_host ~vm_uuid:init_uuid ~label:"last_su" >>= fun () ->
      Storage_backend.set_last_request_timestamp t.storage ~vm_uuid ~dns_name:(Dns.Name.of_string "replica") (Unix.time()) >>= fun () ->
      modify_nor t ~host:init_host ~vm_uuid:init_uuid ~value:1
    in
    let remote_replicate (init_uuid, init_host (* type? *), appid, vm_stop_mode, kernel) remote_host =
      t.log "remote build function";
      let storage = remote_host.DC.irmin_store in
      Storage_backend.request_replica storage ~init_vm_uuid:init_uuid ~init_host ~appid ~vm_stop_mode ~kernel
    in
    let extract_params_from_vm vm_uuid =
      print_endline (yellow "extract params from vm: %s\n" (Uuidm.to_string vm_uuid));
      lwt init_uuid =
        Storage_backend.get_init_uuid t.storage ~vm_uuid >>= function
        | None -> return vm_uuid
        | Some u -> return u
      in
      Storage_backend.get_init_host t.storage ~vm_uuid >>= fun init_host ->
      Storage_backend.get_appid t.storage ~vm_uuid >>= fun appid ->
      Storage_backend.get_stop_mode t.storage ~vm_uuid >>= fun stop_mode ->
      or_vm_backend_error
        "Unable to get VM kernel" (Vm_backend.get_kernel t.vm_backend) vm_uuid >>= fun kernel ->
      return ( init_uuid, init_host, appid, stop_mode, kernel )
    in
    let extract_params_from_irmin () =
      t.log (Printf.sprintf "extract params from irmin\n size of params: %d\n\n" (List.length params));
      (* TODO: return error when the number of parameters is not correct *)
      let init_uuid =
        match Uuidm.of_string (Rpc.string_of_rpc (List.nth params 0)) with (* TODO: store application name in irmin so a VM cannot send a fake one *)
          | None -> raise (Failure (red "No initial UUID found"))
          | Some uuid -> uuid
      in
      let init_host = Rpc.string_of_rpc (List.nth params 1) in
      let appid    = Rpc.int_of_rpc (List.nth params 2) in
      let stop_mode = Vm_stop_mode.of_string (Rpc.string_of_rpc (List.nth params 3)) in
      let kernel = Uri.of_string (Rpc.string_of_rpc (List.nth params 4)) in
      return ( init_uuid, init_host, appid, stop_mode, kernel )
    in
    match DC.resource_available () with
    | true -> begin
      lwt prms =
        match DC.is_host t.datacenter requestor with
        | true -> extract_params_from_irmin ()
        | false ->
          lwt vm_uuid =
            let msg = Printf.sprintf "No UUID match for the VM %s" requestor in
            lwt_the msg (Storage_backend.get_uuid_of_mac t.storage ~vm_mac:(Macaddr.of_string_exn requestor))
          in
            extract_params_from_vm vm_uuid
      in
        let (init_uuid, init_host, _, _, _) = prms in
        lwt time =
          lwt last_su_time = get_timestamp t ~host:init_host ~vm_uuid:init_uuid ~label:"last_su" in
          match last_su_time with
          | None ->
            t.log (Printf.sprintf "extract start time for %s" (Uuidm.to_string init_uuid));
            lwt_the "start timestamp empty" (Storage_backend.get_start_timestamp t.storage ~vm_uuid:init_uuid)
          | Some tm -> return tm
        in
          match ((Unix.time () -. time) > Dc_params.safeguard_time) with
          | true -> local_replicate prms
          | false -> return (t.log ("Not the right time to replicate"))
    end
    | false -> begin
      match DC.is_host t.datacenter requestor with
      | true -> return () (* TODO: write response *)
      | false ->
        lwt vm_uuid =
          let msg = Printf.sprintf "No UUID match for the VM %s" requestor in
          lwt_the msg (Storage_backend.get_uuid_of_mac t.storage ~vm_mac:(Macaddr.of_string_exn requestor))
        in
        extract_params_from_vm vm_uuid >>= fun prms ->
        let (init_uuid, init_host, _, _, _) = prms in
        lwt last_su_time = get_timestamp t ~host:init_host ~vm_uuid:init_uuid ~label:"last_su" in
        match last_su_time with
        | None -> raise (Failure (Printf.sprintf "last_su: Not found for initial VM %s on %s" (Uuidm.to_string init_uuid) init_host))
        | Some time ->
          match ((Unix.time () -. time) > Dc_params.safeguard_time) with
          | true -> remote_replicate prms (DC.get_a_remote_host t.datacenter)
          | false -> return (t.log ("Not the right time to replicate"))
    end

  let dis_replica t ~requestor =
    config_ovs t requestor Disable () >>= fun _ ->
    return ()

  let del_replica t ~requestor =
    lwt vm_uuid =
      let msg = Printf.sprintf "No UUID match for the VM %s" requestor in
      lwt_the msg (Storage_backend.get_uuid_of_mac t.storage ~vm_mac:(Macaddr.of_string_exn requestor))
    in
    lwt init_uuid =
      lwt_the "No initial UUID found" (Storage_backend.get_init_uuid t.storage ~vm_uuid)
    in
    Storage_backend.get_init_host t.storage ~vm_uuid >>= fun init_host ->
    lwt last_sd_time = get_timestamp t ~host:init_host ~vm_uuid:init_uuid ~label:"last_sd" in
    lwt time =
      lwt last_su_time = get_timestamp t ~host:init_host ~vm_uuid:init_uuid ~label:"last_sd" in
      match last_su_time with
      | None ->
        t.log (Printf.sprintf "extract start time for %s" (Uuidm.to_string init_uuid));
        lwt_the "start timestamp empty" (Storage_backend.get_start_timestamp t.storage ~vm_uuid:init_uuid)
       | Some tm -> return tm
    in
    match ((Unix.time () -. time) > Dc_params.safeguard_time) with
    | true -> begin
      or_vm_backend_error "Unable to lookup VM by name"
        (Vm_backend.lookup_vm_by_name t.vm_backend) requestor >>= fun vm_uuid ->
      Storage_backend.set_ttl t.storage ~vm_uuid ~dns_name:(Dns.Name.of_string "replica") 0 >>= fun () ->
      set_timestamp t ~host:init_host ~vm_uuid:init_uuid ~label:"last_sd" >>= fun () ->
      modify_nor t ~host:init_host ~vm_uuid:init_uuid ~value:-1
    end
    | false -> return (t.log "Not the right time to destroy")


  let startup_check t = (* TODO: discuss the policy *)
    Storage_backend.get_vm_list t.storage >>= fun uuids ->
    Lwt_list.iter_s ( fun vm_uuid ->
      Storage_backend.get_vm_name t.storage vm_uuid >>= fun vm_name ->
      or_vm_backend_error "Backup VM not found!" (Vm_backend.lookup_vm_by_name t.vm_backend) vm_name >>= fun vm_uuid' ->
      Storage_backend.replace_uuids t.storage vm_uuid vm_uuid' >>= fun () ->
      return ()
    ) uuids
    >>= fun () ->
    (* TODO: clean OVS *)
    Storage_backend.get_vm_list t.storage >>= fun vm_name_list ->
    Lwt_list.filter_map_s (fun vm_uuid ->
      lwt irmin_state = Storage_backend.get_vm_state t.storage ~vm_uuid in
      match irmin_state with
      | Vm_state.Running -> (* VM crashed *)
        get_vm_state t vm_uuid >>= fun vm_state -> begin
        match vm_state with
        | Vm_state.Running ->
          vm_reboot_check t vm_uuid >>= fun () -> Lwt.return_none
        | _ -> (* VM already stopped/paused/crashed.. *)
          Storage_backend.get_init_uuid t.storage ~vm_uuid >>= begin function
          | None ->
            t.log (Printf.sprintf "original VM %s is not running" (Uuidm.to_string vm_uuid));
            start_vm t vm_uuid >>= fun () -> (* TODO: set the number of replicas to 0 *)
            Lwt.return_none
          | Some _ -> Lwt.return (Some vm_uuid)
          end
        end
      | _ ->
        Storage_backend.get_init_uuid t.storage ~vm_uuid >>= begin function
        | None -> Lwt.return_none
        | Some _ ->
          (* shutdown non-initial VMs ready to become deleted (undefined) *)
          or_vm_backend_error "Unable to shutdown VM" (Vm_backend.shutdown_vm t.vm_backend) vm_uuid >>= fun () ->
          Lwt.return (Some vm_uuid)
        end
    ) vm_name_list >>= fun crashed_vms ->
    Lwt_list.iter_s (fun vm_uuid ->
      t.log (Printf.sprintf "VM %s is crashed" (Uuidm.to_string vm_uuid));
      or_vm_backend_error "Unable to get MAC for VM" (Vm_backend.get_mac t.vm_backend) vm_uuid >>= function
      | m::_ -> begin
        (* delete (undefine) VM *)
        or_vm_backend_error "Unable to undefine VM" (Vm_backend.undefine_vm t.vm_backend) vm_uuid >>= fun () ->
        Storage_backend.delete_vm t.storage ~vm_uuid ~vm_mac:m
        (* what happenes to other replicas on other hosts? *)
      end
      | [] -> raise (Failure (red "VM %s has no MAC address. OVS not updated." (Uuidm.to_string vm_uuid)))
    ) crashed_vms
end
