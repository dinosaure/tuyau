module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) = struct
  open E2

  module Identifier = struct
    type t = E1.identifier

    let compare = E1.identifier_compare
    let equal = E1.identifier_equal
  end

  module Service = Service.Make (IO) (B)

  module Scheme : sig
    type 'a t = private string

    val of_string : string -> ('a t, [ `Msg of string ]) result
    val of_string_exn : string -> 'a t
    val to_string : 'a t -> string
  end = struct
    type 'a t = string

    let of_string x : ('a t, [ `Msg of string ]) result = Ok x
    let of_string_exn x : 'a t = match of_string x with
      | Ok x -> x
      | Error (`Msg err) -> invalid_arg err
    let to_string : 'a t -> string = fun x -> x
  end

  module Value = struct type 'a t = 'a Service.scheme end

  module Hashtbl = Make (Scheme) (Value)

  type 'a scheme = 'a Hashtbl.key

  let make (type v) ~scheme : v scheme = Hashtbl.Key.create scheme

  let string_of_scheme : 'a scheme -> string = fun resolver ->
    Scheme.to_string (Hashtbl.Key.info resolver)

  type t = Hashtbl.t

  let create length = Hashtbl.create length
  let add t scheme witness = Hashtbl.add t scheme witness
  let get k t = Hashtbl.find k t

  let get_service_by_scheme
    : type v.  t -> v scheme -> v Service.scheme option IO.t
    = fun tbl key -> match Hashtbl.find tbl key with
      | scheme -> IO.return (Some scheme)
      | exception Not_found -> IO.return None
end
