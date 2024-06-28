(**************************************************************************)
(*                                                                        *)
(*  This file is part of the why3find.                                    *)
(*                                                                        *)
(*  Copyright (C) 2022-2024                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the enclosed LICENSE.md for details.                              *)
(*                                                                        *)
(**************************************************************************)

[@@@ warning "-32"]

(* -------------------------------------------------------------------------- *)
(* --- Socket Bytes Read & Write                                          --- *)
(* -------------------------------------------------------------------------- *)

type channel = {
  sock : Unix.file_descr ; (* Socket *)
  snd  : bytes ; (* SND bytes buffer, re-used for transport *)
  rcv  : bytes ; (* RCV bytes buffer, re-used for transport *)
  brcv : Buffer.t ; (* RCV data buffer, accumulated *)
  bsnd : Buffer.t ; (* SND data buffer, accumulated *)
}

let read_bytes { sock ; rcv ; brcv } =
  (* rcv buffer is only used locally *)
  let s = Bytes.length rcv in
  let rec scan p =
    (* try to fill RCV buffer *)
    let n =
      try Unix.read sock rcv p (s-p)
      with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> 0
    in
    let p = p + n in
    if n > 0 && p < s then scan p else p
  in
  let n = scan 0 in
  if n > 0 then Buffer.add_subbytes brcv rcv 0 n

let send_bytes { sock ; snd ; bsnd } =
  (* snd buffer is only used locally *)
  let n = Buffer.length bsnd in
  if n > 0 then
    let s = Bytes.length snd in
    let rec send p =
      (* try to flush BSND buffer *)
      let w = min (n-p) s in
      Buffer.blit bsnd p snd 0 w ;
      let r =
        try Unix.single_write sock snd 0 w
        with Unix.Unix_error((EAGAIN|EWOULDBLOCK),_,_) -> 0
      in
      let p = p + r in
      if r > 0 && p < n then send p else p
    in
    let p = send 0 in
    if p > 0 then
      let tail = Buffer.sub bsnd p (n-p) in
      Buffer.reset bsnd ;
      Buffer.add_string bsnd tail

(* -------------------------------------------------------------------------- *)
(* --- Launch Server                                                      --- *)
(* -------------------------------------------------------------------------- *)

let run ~port =
  begin
    Log.message "LSP Server on localhost:%d" port ;
    let fd = Unix.socket PF_INET SOCK_STREAM 0 in
    let localhost = Unix.inet_addr_of_string "127.0.0.1" in
    Unix.bind fd (ADDR_INET(localhost,port)) ;
    Unix.listen fd 1 ;
    ignore (Sys.signal Sys.sigpipe Signal_ignore) ;
    Log.message "Running…"
  end
