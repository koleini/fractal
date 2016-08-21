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

module Make : functor (Storage_backend : Backends.STORAGE_BACKEND) ->
  sig

    type host = {
      name : string;
      ip : Ipaddr.V4.t;
      irmin_store : Storage_backend.t;
      controller : NaasLoad.t option;
    }

    type t = host list

    val create_host : string -> Ipaddr.V4.t -> Uri.t -> host option Lwt.t

    val create : unit -> t Lwt.t

    val get_local_host : t -> host
    val get_all_remote_hosts : t -> host list
    val get_a_remote_host : t -> host
    val get_host_ip : t -> string -> Ipaddr.V4.t
    val get_irmin_uri : unit -> Uri.t
    val find_host_by_name : t -> string -> host
    val is_host : t -> string -> bool
    val resource_available : unit -> bool

end