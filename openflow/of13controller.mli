open OpenFlow0x04_Core

type t

val int64_of_macaddr: Macaddr.t -> int64

val init_controller : ?of_port:int -> Ipaddr.V4.t -> t Lwt.t
val wait: t -> unit Lwt.t

val send_flow_mod: t -> int -> oxmMatch -> instruction list -> unit Lwt.t
val send_flow_mod_del: t -> oxmMatch -> unit Lwt.t

val extract_service_ip: OpenFlow0x04_Core.bucket list -> int32  * Packet.int48

val add_vm: t -> ?ext_port:OpenFlow0x04_Core.portId -> Ipaddr.V4.t -> Macaddr.t -> 
    OpenFlow0x04_Core.portDesc -> unit Lwt.t

val get_port: t -> string -> OpenFlow0x04_Core.portDesc Lwt.t

val get_group_buckets: t -> int32 -> OpenFlow0x04_Core.bucket list Lwt.t
val get_group_stats: t -> int32 -> OpenFlow0x04_Core.groupStats list option Lwt.t
val init_of_group: t -> int32 -> Ipaddr.V4.t -> Macaddr.t -> int32 -> unit Lwt.t
val append_of_group_bucket: t -> int32 -> Ipaddr.V4.t -> Macaddr.t -> actionSequence -> unit Lwt.t
val disable_of_group_bucket: t -> int32 ->  OpenFlow0x04_Core.portId -> unit Lwt.t
val delete_of_group_bucket: t -> int32 ->  OpenFlow0x04_Core.portId -> unit Lwt.t
