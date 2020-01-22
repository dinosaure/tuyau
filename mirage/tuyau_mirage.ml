module Mirage_scheduler = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f
  let return x = Lwt.return x
end

include Tuyau.Make(Mirage_scheduler)(Cstruct)(Cstruct)

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
