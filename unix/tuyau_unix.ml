module Unix_scheduler = struct
  type +'a t = 'a

  let bind x f = f x
  let return x = x
end

include Tuyau.Make(Unix_scheduler)(Bytes)(String)
