open Tuyau_unix

type endpoint = Unix.sockaddr
type protocol

val endpoint : endpoint key
val protocol : protocol Witness.protocol
val socket : protocol -> Unix.file_descr

type configuration =
  { inet_addr : Unix.inet_addr
  ; port : int
  ; capacity : int }

type service

val configuration : configuration key
val service : (service * protocol) Witness.service
