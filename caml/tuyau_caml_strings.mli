type flow =
  { mutable input : string list
  ; output : string Queue.t }

val strings : string list Tuyau_caml.key
val strings_protocol : flow Tuyau_caml.Witness.protocol
