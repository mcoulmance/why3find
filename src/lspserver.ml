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
  mutable offset : int ; (* no end-of-line detected before *)
  rcv : bytes ; (* RCV bytes buffer, re-used for transport *)
  snd : bytes ; (* SND bytes buffer, re-used for transport *)
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
(* --- Fetching                                                           --- *)
(* -------------------------------------------------------------------------- *)

let input (channel:channel) =
  begin
    let { brcv } = channel in
    let n = Buffer.length brcv in
    let rec lookup p =
      if p < n then
        if Buffer.nth brcv p <> '\n' then lookup (p+1)
        else
          begin
            let q =
              if p > 0 && Buffer.nth brcv (p-1) = '\r' then p-1 else p in
            let line = Buffer.sub brcv 0 q in
            let tail = Buffer.sub brcv (p+1) (n-p-1) in
            channel.offset <- 0 ;
            Buffer.reset brcv ;
            Buffer.add_string brcv tail ;
            Some line
          end
      else ( channel.offset <- p ; None )
    in lookup channel.offset
  end

let rec fetch channel =
  send_bytes channel ;
  read_bytes channel ;
  match input channel with
  | Some line -> line
  | None -> Unix.sleepf 0.01 ; (fetch [@tailcall]) channel

(* -------------------------------------------------------------------------- *)
(* --- Running LSP                                                        --- *)
(* -------------------------------------------------------------------------- *)

let lsp (channel:channel) =
  begin
    Log.message "Running…" ;
    while true do
      let request = fetch channel in
      let data = fetch channel in
      Log.message "Rq %S [%d]" request (String.length data) ;
    done
  end

(* -------------------------------------------------------------------------- *)
(* --- Launch Server                                                      --- *)
(* -------------------------------------------------------------------------- *)

type transport =
  | PIPE of string
  | PORT of int

let socket = function
  | PORT port ->
    Log.message "Socket port %d" port ;
    let fd = Unix.socket ~cloexec:true PF_INET SOCK_STREAM 0 in
    Unix.setsockopt fd SO_REUSEADDR true ;
    let localhost = Unix.inet_addr_of_string "0.0.0.0" in
    Unix.bind fd (ADDR_INET(localhost,port)) ; fd
  | PIPE file ->
    Log.message "Socket pipe %s" file ;
    if Sys.file_exists file then Unix.unlink file ;
    let fd = Unix.socket PF_UNIX SOCK_STREAM 0 in
    Unix.bind fd (ADDR_UNIX file) ; fd

let set_socket_size sock opt s =
  begin
    let nbytes = s * 1024 in
    begin
      try Unix.setsockopt_int sock opt nbytes
      with Unix.Unix_error(err,_,_) ->
        let msg = Unix.error_message err in
        Log.warning "Invalid socket size (%d: %s)" nbytes msg ;
    end ;
    Unix.getsockopt_int sock opt
  end

let run transport =
  begin
    Log.message "Why3 LSP Server" ;
    let fd = try socket transport with exn ->
      Log.error "Socket: %s" (Printexc.to_string exn) ;
      exit 1 in
    Unix.listen fd 1 ;
    while true do
      try
        Log.message "Waiting..." ;
        let sock,_addr = Unix.accept ~cloexec:true fd in
        Unix.set_nonblock sock ;
        let rcv = set_socket_size sock SO_RCVBUF 256 in
        let snd = set_socket_size sock SO_SNDBUF 256 in
        let channel = {
          sock ;
          offset = 0 ;
          snd = Bytes.create snd ;
          rcv = Bytes.create rcv ;
          bsnd = Buffer.create snd ;
          brcv = Buffer.create rcv ;
        } in
        Log.message "Client connected." ;
        lsp channel ;
      with
      | Unix.Unix_error(EAGAIN,_,_) ->
        Unix.sleepf 0.1 ; (* 100ms *)
      | Unix.Unix_error(EPIPE,_,_) ->
        Log.message "Client disconnected." ;
        Unix.close fd ; exit 0
      | exn ->
        Log.error "Protocol error: %s" (Printexc.to_string exn) ;
        Unix.close fd ; exit 1
    done
  end
