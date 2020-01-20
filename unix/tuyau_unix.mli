module Unix_scheduler : Tuyau.Sigs.SCHEDULER with type +'a t = 'a

include Tuyau.S
  with type input = Bytes.t
   and type output = String.t
   and type +'a s = 'a
