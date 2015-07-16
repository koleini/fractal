type t

val init_controller : ?ovsdb_port:int -> Ipaddr.V4.t -> t Lwt.t

val db_add_tunnel: t -> string -> Ipaddr.V4.t -> string -> int -> unit Lwt.t
val db_del_tunnel: t -> string -> Ipaddr.V4.t -> string -> int -> unit Lwt.t
