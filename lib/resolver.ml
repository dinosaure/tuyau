module Make (IO : Sigs.IO) = struct
  open E1
  open IO

  module type RESOLVER = Sigs.FUNCTOR with type 'a t = Domain_name.t -> 'a option IO.t

  module Desc = struct type 'a t = { name : string } end
  module Resolver : RESOLVER = struct type 'a t = Domain_name.t -> 'a option IO.t end
  module Map = Make (Desc) (Resolver)

  type 'a resolver = 'a Map.key

  let make (type v) ~name : v resolver = Map.Key.create { Desc.name }

  let name : 'a resolver -> string = fun resolver ->
    let { Desc.name } = Map.Key.info resolver in name

  type t = Map.t

  let empty = Map.empty
  let add k ~resolve t = Map.add k resolve t
  let rem = Map.rem
  let get k t = match Map.find k t with
    | Some v -> v
    | None -> raise Not_found

  let resolve : type v. Domain_name.t -> v resolver -> Map.t -> v option IO.t =
    fun domain key m -> match Map.find key m with
      | Some resolver -> resolver domain
      | None -> return None
end
