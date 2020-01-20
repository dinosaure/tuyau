open Tuyau_unix

type tcp

val sockaddr : Unix.sockaddr key
val tcp_protocol : tcp Witness.protocol

type configuration =
  { inet_addr : Unix.inet_addr
  ; port : int
  ; capacity : int }

type master

val configuration : configuration key
val tcp_service : (master * tcp) Witness.service
