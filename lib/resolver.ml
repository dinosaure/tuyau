module Make (IO : Sigs.IO) = struct
  open E1
  open IO

  module Identifier = struct
    type t = E1.identifier

    let compare = E1.identifier_compare
    let equal = E1.identifier_equal
  end

  module type RESOLVER = Sigs.FUNCTOR
    with type 'a t = Identifier.t list -> Domain_name.t -> 'a option IO.t

  module Desc = struct type 'a t = { name : string } end

  module Resolver : RESOLVER = struct
    type 'a t = Identifier.t list -> Domain_name.t -> 'a option IO.t
  end

  module Map = Make (Desc) (Resolver)

  type 'a resolver = 'a Map.key

  let make (type v) ~name : v resolver = Map.Key.create { Desc.name }

  let name : 'a resolver -> string = fun resolver ->
    let { Desc.name } = Map.Key.info resolver in name

  let identifier : 'a resolver -> Identifier.t = fun resolver ->
    Map.Key.identifier resolver

  type t = Map.t

  let empty = Map.empty
  let add k ~resolve t = Map.add k resolve t
  let rem = Map.rem
  let get k t = match Map.find k t with
    | Some v -> v
    | None -> raise Not_found

  let resolve
    : type v. ?colored:identifier list -> Domain_name.t -> v resolver -> Map.t -> v option IO.t
    = fun ?(colored = []) domain key m -> match Map.find key m with
      | Some resolver -> resolver colored domain
      | None -> return None
end
