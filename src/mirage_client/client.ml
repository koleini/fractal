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
open Irmin_unix 

let red fmt    = sprintf ("\027[31m"^^fmt^^"\027[m")
let green fmt  = sprintf ("\027[32m"^^fmt^^"\027[m")
let yellow fmt = sprintf ("\027[33m"^^fmt^^"\027[m")
let blue fmt   = sprintf ("\027[36m"^^fmt^^"\027[m")
 
exception Connect_error 
 
let dst_ip = "127.0.0.1"
let dst_port = 8080

module Main (C:CONSOLE)(COM:STACKV4) = struct

  module TCOM  = COM.TCPV4

  let start console com =

  Irmin_backend.create ~persist:false ~root:"/tmp/jitsu" ~uri:(Uri.of_string Sys.argv.(2)) () >>= fun storage ->

  let check_cmd_args cmd count = 
    if ((Array.length Sys.argv) < (2 + count)) then
      failwith (sprintf "Insufficient args for command %s (required %d)"
                cmd count)
  in
  let send_cmd () = 
  try_lwt
    if ((Array.length Sys.argv) < 2) then 
      failwith "No command defined";
 
    match (Sys.argv.(1)) with
    (* | "define_vm" -> 
         let _ = check_cmd_args Sys.argv.(1) 2 in *)
      | "add_replica" ->
        let _ = check_cmd_args Sys.argv.(1) 0 in 
          Irmin_backend.add_replica storage ~vm_name:Sys.argv.(3) ~app_name:Sys.argv.(4) 
            ~app_id:(int_of_string Sys.argv.(5)) ~vm_stop_mode:(Vm_stop_mode.of_string Sys.argv.(6))
            ~ttl:(int_of_string Sys.argv.(7)) ~kernel:(if Sys.argv.(8) = "" then Uri.empty else Uri.of_string Sys.argv.(8)) (* (Uri.of_string Sys.argv.(8)) *)
            
         (* Irmin_backend.add_replica storage ~vm_name:Sys.argv.(2) ~app_name:Sys.argv.(3) 
            ~app_id:(int_of_string Sys.argv.(4)) ~vm_stop_mode:(Vm_stop_mode.of_string Sys.argv.(5))
            ~ttl:(int_of_string Sys.argv.(6)) ~kernel:Uri.empty (* (Uri.of_string Sys.argv.(7)) *)
        *)
        (* | "del_replica" ->
             let _ = check_cmd_args Sys.argv.(1) 0 in
               [ Rpc.String Sys.argv.(2);
               ] *)
      | "del_replica" ->
        let _ = check_cmd_args Sys.argv.(1) 0 in
          Irmin_backend.del_replica storage ~vm_name:Sys.argv.(3) ~app_name:Sys.argv.(4)
        (* | "del_replica" -> 
             let _ = check_cmd_args Sys.argv.(1) 0 in
               [ Rpc.String Sys.argv.(2);
               ] *)
      | "define_vm" -> 
        let _ = check_cmd_args Sys.argv.(1) 0 in
          Irmin_backend.define_vm storage ~vm_name:Sys.argv.(3) ~mac:(Macaddr.of_string_exn Sys.argv.(4)) ~kernel:(Uri.of_string Sys.argv.(5))
        (* | "del_replica" ->
             let _ = check_cmd_args Sys.argv.(1) 0 in
               [ Rpc.String Sys.argv.(2);
               ] *)
      | _ -> 
        failwith (sprintf "Fail: unknown cmd: %s\n%!" Sys.argv.(1))
  with  ex -> 
     return (printf "Fail: %s" (Printexc.to_string ex))
  in

  send_cmd ()
end
