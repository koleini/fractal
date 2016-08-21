(*
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

module Make(Storage_backend : Backends.STORAGE_BACKEND) = struct
  module I = Irmin_unix.Irmin_http.Make(Irmin.Contents.String)(Irmin.Ref.String)(Irmin.Hash.SHA1)

  type host = {
    name : string;
    ip : Ipaddr.V4.t;
    irmin_store : Storage_backend.t;
    controller : NaasLoad.t option;
  }

  type t = host list

  let create_host name ip irmin =
      lwt controller = Lwt.return_none (* NaasLoad.create_controller ip *) in
      lwt irmin_store = Storage_backend.create ~address:irmin () in
      match List.hd Dc_params.hosts_names with
      | n when n = name -> Lwt.return (Some { name; ip; irmin_store; controller })
      | _ ->
        let ir_conn = Storage_backend.get_irmin_conn irmin_store in
        let path = [ "jitsu" ; "datacenter"; (List.hd Dc_params.hosts_names) ; ] in
        I.update (ir_conn "Update keep-alive") path (string_of_float (Unix.time ())) >>= fun () ->
        Lwt.return (Some { name; ip; irmin_store; controller })      

  let create () =
    let rec build_hosts names ips irmins =
      match names, ips, irmins with
      | name::n_t, ip::ip_t, irmin::irmin_t ->
        build_hosts n_t ip_t irmin_t >>= fun h_t ->
        create_host name ip irmin >>= begin function
        | Some h -> Lwt.return (h::h_t)
        | None -> Lwt.return h_t
      end
      | [], [], [] -> Lwt.return []
      | _, _, _ -> raise (Failure "Hosts' info in dc_params.ml have lists of different sizes.")
    in
      build_hosts Dc_params.hosts_names Dc_params.hosts_ips Dc_params.hosts_irmins

  let get_local_host t = List.hd t

  let get_all_remote_hosts t = List.tl t

  let get_a_remote_host t =
    (* TODO: replace with selection algorithm *)
    List.nth t 1

  let get_host_ip t (name : string) = (* M : using DNS records? *) 
    (List.find (fun h -> name = h.name) t).ip

  let get_irmin_uri () = List.hd Dc_params.hosts_irmins

  let find_host_by_name t (name : string) =
    List.find (fun h -> h.name = name) t

  let is_host t vm_name = (* check if vm_name is a registered host *)
    List.exists (fun h -> h.name = vm_name) t

  let resource_available () =
    (* TODO: replace with resource checking algorithm *)
    true

end
