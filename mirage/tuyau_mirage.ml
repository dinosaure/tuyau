module Mirage_scheduler = Tuyau_lwt.Lwt_scheduler

include Tuyau_lwt

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
