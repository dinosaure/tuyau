module Mirage_scheduler : Tuyau.Sigs.SCHEDULER with type +'a t = 'a Lwt.t

include Tuyau.S
  with type input = Cstruct.t
   and type output = Cstruct.t
   and type +'a s = 'a Lwt.t

module type T = sig
  type protocol
  type endpoint

  val endpoint : endpoint key
  val protocol : protocol Witness.protocol

  type configuration
  type service

  val configuration : configuration key
  val service : (service * protocol) Witness.service
end
