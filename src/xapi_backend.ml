(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2015 Masoud Koleini <masoud.koleini@nottingham.ac.uk>
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

open Xen_api_lwt_unix

let json = ref false

type t = {
  connection : (Rpc.call -> Rpc.response Lwt.t) * string; (* Xapi connection *)
}

type vm = {
  uuid : string; (* always lookup by uuid to avoid stale identifiers *)
  (*domain : Libvirt.rw Libvirt.Domain.t;  (* Libvirt VM *)*)
}

let try_xapi msg f =
  try_lwt
    lwt result = f () in
    Lwt.return (`Ok result)
  with
  | e -> Lwt.return (`Error (`Unknown (Printf.sprintf "%s: %s" msg (Printexc.to_string e))))
 
let define_vm t ~name_label ~mAC ~pV_kernel =
  try_xapi "Unable to define vm" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt vm = VM.create ~rpc ~session_id
	  ~name_label ~name_description:"" ~user_version:0L
	  ~is_a_template:false
	  ~affinity:"OpaqueRef:NULL"
	  ~memory_target:0L
	  ~memory_static_max:268435456L ~memory_dynamic_max:268435456L
	  ~memory_dynamic_min:134217728L ~memory_static_min:134217728L
	  ~vCPUs_params:[] ~vCPUs_max:1L ~vCPUs_at_startup:1L
	  ~actions_after_shutdown:`destroy ~actions_after_reboot:`restart ~actions_after_crash:`restart
	  ~pV_bootloader:"" ~pV_kernel:(Uri.to_string pV_kernel) ~pV_ramdisk:"" ~pV_args:""
	  ~pV_bootloader_args:"" ~pV_legacy_args:""
	  ~hVM_boot_policy:"" ~hVM_boot_params:[] ~hVM_shadow_multiplier:1.0
	  ~platform:[]
	  ~pCI_bus:""
	  ~other_config:[("vgpu_pci", ""); ("mac_seed", "c8b61c6d-8bc6-3365-ea27-507a3166bb33")] (* TODO: require seed? *)
	  ~recommendations:""
	  ~xenstore_data:[("vm-data", "")]
	  ~ha_always_run:false ~ha_restart_priority:""
	  ~tags:[]
	  ~blocked_operations:[]
	  ~protection_policy:"OpaqueRef:NULL"
	  ~is_snapshot_from_vmpp:false
	  ~appliance:"OpaqueRef:NULL"
	  ~start_delay:0L ~shutdown_delay:0L ~order:0L ~suspend_SR:"OpaqueRef:NULL" ~version:0L
    ~generation_id:"" 
    ~hardware_platform_version:0L
    in
    lwt net = Network.get_by_name_label ~rpc:rpc ~session_id:session_id
                ~label:"experiment" (* "Pool-wide network associated with eth0" *) in
    lwt _vif = VIF.create ~rpc:rpc ~session_id:session_id
      ~device:"0" ~network:(List.hd net) ~vM:vm ~mAC:(Macaddr.to_string mAC) ~mTU:0L
      ~other_config:[] ~qos_algorithm_type:"" ~qos_algorithm_params:[]  ~locking_mode:`network_default
	  ~ipv4_allowed:[] ~ipv6_allowed:[] in
    lwt uuid = VM.get_uuid ~rpc:rpc ~session_id:session_id ~self:vm in
    Lwt.return { uuid }
  )
  
let connect connstr =
  try_xapi "Unable to connect" (fun () -> 
      let uri  = Uri.of_string connstr in
      let schm = match Uri.scheme uri with | Some h -> h | None -> "http" in
      let host = match Uri.host uri with | Some h -> h | None -> "127.0.0.1" in
      let user = match Uri.user uri with | Some u -> u | None -> "root" in
      let pass = match Uri.password uri with | Some h -> h | None -> "" in
      let port = match Uri.port uri with | Some h -> string_of_int h | None -> "80" in
      let rpc = if !json then make_json (schm ^ "://" ^ host ^ ":" ^ port) else make (schm ^ "://" ^ host ^ ":" ^ port) in
      (* Printf.printf "%s %s %s %s\n" schm host user pass; *)
      lwt session_id = Session.login_with_password ~rpc:rpc ~uname:user ~pwd:pass ~version:"1.0" ~originator:"jitsu" in
      Lwt.return { connection = (rpc, session_id) }
    )

let disconnect t =
  try_xapi "Unable to disconnect" (fun () ->
    let (rpc, session_id) = t.connection in
    Session.logout ~rpc:rpc ~session_id:session_id
    )

(* convert vm state to string *)
let xapi_state_to_vm_state = function
  | `Running -> Vm_state.Running
  | `Paused  -> Vm_state.Paused
  | `Halted  -> Vm_state.Off
  | _ -> Vm_state.Unknown

let lookup_vm_by_uuid t vm_uuid =
  (* We use UUID for internal representation, but call lookup anyway to make sure it exists *)
  (* Xapi: multiple domain can share the same name TODO: *)
  try_xapi "Unable lookup VM UUID" (fun () ->
    let (rpc, session_id) = t.connection in
    lwt domains = VM.get_by_name_label ~rpc:rpc ~session_id:session_id ~label:vm_uuid in
    lwt uuid = VM.get_uuid ~rpc:rpc ~session_id:session_id ~self:(List.hd domains) in
    Lwt.return { uuid }
  )

let lookup_vm_by_name t vm_name =
  (* Xapi: multiple domain can share the same name TODO: *)
  try_xapi "Unable lookup VM name" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domains = VM.get_by_name_label ~rpc:rpc ~session_id:session_id ~label:vm_name in
    lwt uuid = VM.get_uuid ~rpc:rpc ~session_id:session_id ~self:(List.hd domains) in
    Lwt.return { uuid }
  )

let get_state t vm =
  try_xapi "Unable to get VM state" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    lwt state = VM.get_power_state ~rpc:rpc ~session_id:session_id ~self:domain in
    Lwt.return (xapi_state_to_vm_state state)
  )

let get_kernel t vm =
  try_xapi "Unable to get VM kernel" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.get_PV_kernel ~rpc:rpc ~session_id:session_id ~self:domain >>= fun k ->
    return (Uri.of_string k)
  )

let destroy_vm t vm =
  try_xapi "Unable to destroy VM" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.destroy ~rpc:rpc ~session_id:session_id ~self:domain
  )

let shutdown_vm t vm =
  try_xapi "Unable to shutdown VM" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.hard_shutdown ~rpc:rpc ~session_id:session_id ~vm:domain
  )

let suspend_vm t vm =
  try_xapi "Unable to suspend VM" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.suspend ~rpc:rpc ~session_id:session_id ~vm:domain
  )

let resume_vm t vm = (* from pause state *)
  try_xapi "Unable to resume VM" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.unpause ~rpc:rpc ~session_id:session_id ~vm:domain
  )

let unsuspend_vm t vm = (* from suspended state *)
  try_xapi "Unable to unsuspend VM" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.resume ~rpc:rpc ~session_id:session_id ~vm:domain ~start_paused:false ~force:true
  )

let start_vm t vm =
  try_xapi "Unable to start VM" (fun () -> 
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.start ~rpc:rpc ~session_id:session_id ~vm:domain ~start_paused:false ~force:true
  )

(* get mac address for domain - TODO only supports one interface *)

let get_mac t vm =
  try_xapi "Unable to get MAC address for VM" (fun () ->
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    lwt all_vifs = VM.get_VIFs ~rpc:rpc ~session_id:session_id ~self:domain in
    let vif = List.hd all_vifs in (* TODO only supports one interface *)
    lwt mac = VIF.get_MAC ~rpc:rpc ~session_id:session_id ~self:vif in
    Lwt.return (Macaddr.of_string mac)
  )

let get_uuid _ vm =
  Lwt.return (`Ok vm.uuid)

let get_name t vm =
  try_xapi "Unable to get VM name" (fun () ->
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    VM.get_name_label ~rpc:rpc ~session_id:session_id ~self:domain
  )

let get_domain_id t vm =
  try_xapi "Unable to get VM dom id" (fun () ->
    let (rpc, session_id) = t.connection in
    lwt domain = VM.get_by_uuid ~rpc:rpc ~session_id:session_id ~uuid:vm.uuid in
    lwt id = VM.get_domid ~rpc:rpc ~session_id:session_id ~self:domain in
    Lwt.return (Int64.to_int id)
  )

