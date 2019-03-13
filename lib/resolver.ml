module Make (IO : Sigs.IO) = struct
  open E1
  open IO

  module Identifier = struct
    type t = E1.identifier

    let compare = E1.identifier_compare
    let equal = E1.identifier_equal
  end

  module type RESOLVER = Sigs.FUNCTOR
    with type 'a t = Domain_name.t -> 'a option IO.t

  module Desc = struct type 'a t = { name : string } end

  module Resolver : RESOLVER = struct
    type 'a t = Domain_name.t -> 'a option IO.t
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

  let map ~name k f t =
    let resolve = get k t in
    let resolve domain =
      let open IO in resolve domain >>= function
      | Some x -> IO.return (Some (f x))
      | None -> IO.return None in
    let k' = make ~name in
    add k' ~resolve t

  exception Break

  let of_array ~name arr t =
    let resolve domain =
      let res = ref None in
      try Array.iter (fun (domain', v) -> if Domain_name.equal domain domain' then ( res := Some v ; raise Break ) ) arr
        ; IO.return None
      with Break -> IO.return !res in
    let k = make ~name in
    add k ~resolve t

  let of_hashtbl ~name hashtbl t =
    let resolve domain =
      match Hashtbl.find hashtbl domain with
      | v -> IO.return (Some v)
      | exception Not_found -> IO.return None in
    let k = make ~name in
    add k ~resolve t

  let resolve
    : type v. Domain_name.t -> v resolver -> Map.t -> v option IO.t
    = fun domain key m -> match Map.find key m with
      | Some resolver -> resolver domain
      | None -> return None
end
