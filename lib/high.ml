module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) = struct
  module Service = Service.Make (IO) (B)
  module Resolver = Resolver.Make (IO)

  type 'e t =
    { name : string
    ; witness : 'e Service.scheme
    ; endpoint : Service.endpoint }
  and 'a action = 'a Service.action

  let make ~name resolver service endpoint =
    let witness = Service.add resolver service in
    let endpoint = Service.endpoint witness endpoint in
    { name; witness; endpoint }

  let bind : 'e t -> ('e -> 'e t) -> 'e t =
    fun t f ->
      let f e = let { endpoint; _ } = f e in endpoint in
      match Service.bind t.witness t.endpoint f with
      | Some endpoint -> { t with endpoint }
      | None -> t

  let map : 'a t -> ('a -> 'a) -> 'a t =
    fun t f ->
      match Service.map t.witness t.witness t.endpoint f with
      | Some endpoint -> { t with endpoint }
      | None -> assert false (* impossible case *)

  let action : 'e t -> ('e action -> 'a IO.t) -> 'a IO.t =
    fun t f -> Service.instance t.witness f

  let resolve : Domain_name.t -> Resolver.t -> 'e t -> 'e t option IO.t =
    fun domain resolver t ->
      IO.bind (Service.resolve domain resolver t.witness t.endpoint) @@ function
      | Some endpoint -> IO.return (Some { t with endpoint })
      | None -> IO.return None
end

