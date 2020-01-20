module Caml_scheduler = struct
  type 'a t = 'a

  let bind x f = f x
  let return x = x
end

include Tuyau.Make (Caml_scheduler)(Bytes)(String)
