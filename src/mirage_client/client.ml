(*
 * Copyright (C) 2015 University of Nottingham <masoud.koleini@nottingham.ac.uk>
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
 *
 *)

open Lwt
open V1_LWT
open Printf

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")

exception Connect_error

let dst_ip = "127.0.0.1"
let dst_port = 5005

module Main (C:CONSOLE)(COM:STACKV4) = struct

  module TCOM  = COM.TCPV4

  let start console com =

  let check_cmd_args cmd count = 
    if ((Array.length Sys.argv) < (2 + count)) then
      failwith (sprintf "Insufficient args for command %s (required %d)"
                cmd count)
  in
  let send_cmd flow =
  try_lwt
    if ((Array.length Sys.argv) < 2) then 
      failwith "No command defined";

    lwt resp =
      let params =
        match (Sys.argv.(1)) with
        | "define_vm" -> 
             let _ = check_cmd_args Sys.argv.(1) 2 in
               [ Rpc.String Sys.argv.(2);
                 Rpc.String Sys.argv.(3);
                 Rpc.String Sys.argv.(4);
               ]
        | "add_replica" -> 
             let _ = check_cmd_args Sys.argv.(1) 0 in
               [ Rpc.String Sys.argv.(2);
               ]
        | "del_replica" -> 
             let _ = check_cmd_args Sys.argv.(1) 0 in
               [ Rpc.String Sys.argv.(2);
               ]
        | _ -> 
            failwith (sprintf "Fail: unknown cmd: %s\n%!" Sys.argv.(1))
      in
        let cmd = Rpc.({name=Sys.argv.(1); params;}) in 
        lwt _ = TCOM.write flow (Cstruct.of_string ((Jsonrpc.string_of_call cmd) ^ "\n")) in 
        TCOM.read flow
        >>= fun read -> begin
        match read with
        | `Ok resp -> let r = Jsonrpc.response_of_string (Cstruct.to_string resp) in
              return ( "Rpc success: " ^ (string_of_bool r.Rpc.success) ^ "\n" ^
                       "Rpc contents: " ^
                       (Rpc.to_string r.Rpc.contents)
                     )
         | `Eof -> TCOM.close flow >>
                     C.log_s console (red "connection closed") >>
                     return ("connection closed")
         | `Error e -> C.log_s console (red "read: error") >>
                     return ("read: error")
         end
     in
     let _ = printf "result:\n%s\n%!" resp in 
      return () 
  with  ex -> 
     return (printf "Fail: %s" (Printexc.to_string ex))
  in

  let connect dst port =
    COM.TCPV4.create_connection (COM.tcpv4 com) (dst, port) >>= function
    | `Ok outfl -> C.log_s console (red "connected")
                   >> return outfl
    | `Error e -> fail (Failure "failed to connect")
  in
  let send f buf =
    TCOM.write f buf
  in
  lwt flow = connect (Ipaddr.V4.of_string_exn dst_ip) dst_port in
  send_cmd flow >>
  TCOM.close flow
end
