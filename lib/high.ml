module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) = struct
  module Service = Service.Make (IO) (B)
  module Resolver = Resolver.Make (IO)

  type 'f t =
    { name : string
    ; witness : 'f Service.scheme
    ; flow : Service.flow }
  and 'f flow = (module Service.FLOW with type flow = 'f)

  let register ~name resolver service flow =
    let witness = Service.register resolver service in
    let flow = Service.flow witness flow in
    { name; witness; flow }

  let bind : 'e t -> ('e -> 'e t) -> 'e t =
    fun t f ->
      let f e = let { flow; _ } = f e in flow in
      match Service.bind t.witness t.flow f with
      | Some flow -> { t with flow }
      | None -> t

  let map : 'a t -> ('a -> 'a) -> 'a t =
    fun t f ->
      match Service.map t.witness t.witness t.flow f with
      | Some flow -> { t with flow }
      | None -> assert false (* impossible case *)

  let extract : 'f t -> ('f -> 'f flow -> 'a IO.t) -> ('a IO.t, [ `Msg of string]) result =
    fun t f -> Service.extract t.witness t.flow f

  let resolve : Domain_name.t -> Resolver.t -> 'e t -> ('e t, [ `Unresolved | `Msg of string ]) result IO.t =
    fun domain resolver t ->
      IO.bind (Service.resolve domain resolver t.witness t.flow) @@ function
      | Ok flow -> IO.return (Ok { t with flow })
      | Error _ as err -> IO.return err
end

