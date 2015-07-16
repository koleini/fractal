type t

val create_controller: Ipaddr.V4.t -> t Lwt.t
val wait: t -> unit Lwt.t

val add_app_of13: t -> int32 -> Ipaddr.V4.t -> Macaddr.t -> int -> Ipaddr.V4.t -> unit Lwt.t
val add_replica_of13: t -> t -> int32 -> Ipaddr.V4.t -> Macaddr.t -> int ->  unit Lwt.t
val disable_replica_of13: t -> t -> int32 -> Ipaddr.V4.t -> int ->  unit Lwt.t
val delete_replica_of13: t -> t -> int32 -> Ipaddr.V4.t -> int ->  unit Lwt.t
