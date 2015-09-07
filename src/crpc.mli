

val socket : Unix.sockaddr

val c_function : string array -> Lwt_io.input_channel * Lwt_io.output_channel -> Rpc.response Lwt.t