module Make (IO : Sigs.IO) = struct
  module Resolver = Resolver.Make(IO)

  module Option = struct
    type 'a t = 'a option IO.t

    let return v = IO.return (Some v)
    let map f a = let open IO in a >|= function Some v -> Some (f v) | None -> None
    let (>>|) a f = map f a
  end

  open Resolver

  type identifier = Resolver.Identifier.t
  type 'a impl = Domain_name.t -> 'a option IO.t
  type epsilon = unit

  type ('ty, 'v, 'a, 'b) order =
    | Choose : 'a resolver * 'a resolver -> ('v, 'v, 'a, 'a) order
    | Using : (('a -> 'b) -> 'v, 'v, 'a, 'b) order
    | Rename : (string -> 'v, 'v, 'a, 'a) order
    | Resolver : 'b resolver -> ('b impl -> 'v, 'v, 'a, 'b) order
    | Empty : ('v, 'v, epsilon, epsilon) order

  type ('ty, 'v, 'a, 'b) fmt =
    | [] : ('v, 'v, 'a, 'a) fmt
    | (::) : ('x, 'v, 'a, 'b) order * ('v, 'r, 'b, 'c) fmt -> ('x, 'r, 'a, 'c) fmt

  type t = Resolver.t

  let none : epsilon resolver = Resolver.make ~name:"epsilon"

  let exists key colored =
    List.exists (Resolver.Identifier.equal (Resolver.identifier key)) colored

  let keval_order
    : type ty v a b. t -> key:a resolver -> (ty, v, a, b) order -> (t * b resolver -> v) -> ty
    = fun t ~key o k -> match o with
      | Empty -> k (t, key)
      | Resolver resolver ->
        fun resolve ->
          let resolve colored domain =
            if exists resolver colored
            then IO.return None
            else resolve domain in
          k (Resolver.add resolver ~resolve t, resolver)
      | Rename ->
        fun name ->
          let resolve = Resolver.get key t in
          let key = Resolver.make ~name in
          let resolve colored domain =
            if exists key colored
            then IO.return None
            else resolve (Resolver.identifier key :: colored) domain in
          k (Resolver.add key ~resolve t, key)
      | Choose (ka, kb) ->
        let a = Resolver.get ka t in
        let b = Resolver.get kb t in
        let resolve colored domain =
          let open IO in
          a colored domain >>= function
          | Some _ as v -> return v
          | None -> b (Resolver.identifier ka :: colored) domain in
        k (Resolver.add key ~resolve t, key)
      | Using ->
        fun apply ->
          let resolve = Resolver.get key t in
          let key' = Resolver.make ~name:"using" in
          let resolve colored domain =
            if exists key' colored
            then IO.return None
            else let open Option in resolve (Resolver.identifier key' :: colored) domain >>| apply in
          k (Resolver.add key' ~resolve t, key')

  let rec keval
    : type ty v a b. t -> key:a resolver -> (ty, v, a, b) fmt -> (t * b resolver -> v) -> ty
    = fun t ~key l k -> match l with
      | [] -> k (t, key)
      | x :: r ->
        let k' (t, key) = keval t ~key r k in
        keval_order t ~key x k'

  let eval (t, key) fmt = keval t ~key fmt (fun (t, key) -> (t, key))
  let v name = Resolver (Resolver.make ~name)
  let using = Using
  let rename = Rename

  let (<|>) (ta, a) (tb, b) =
    let key = Resolver.make ~name:"choose" in
    let merge _ _ = assert false in
    let identity x = x in
    keval_order (merge ta tb) ~key (Choose (a, b)) identity

  let empty = Resolver.empty, none

  let t, k =
    (eval empty [ v "foo"; using ; rename ]
       (fun _ -> Option.return 42)
       string_of_int
       "string_of_foo")
    <|>
    (eval empty [ v "bar" ]
       (fun _ -> Option.return "localhost"))
end
