module Lwt_scheduler = struct
  type +'a t = 'a Lwt.t

  let bind x f = Lwt.bind x f
  let return x = Lwt.return x
end

include Tuyau.Make(Lwt_scheduler)(Cstruct)(Cstruct)
