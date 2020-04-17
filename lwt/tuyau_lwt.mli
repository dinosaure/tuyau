module Lwt_scheduler : Tuyau.Sigs.SCHEDULER with type +'a t = 'a Lwt.t

include Tuyau.S
  with type input = Cstruct.t
   and type output = Cstruct.t
   and type +'a s = 'a Lwt.t
