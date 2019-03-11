module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) = struct
  module Service = Service.Make (IO) (B)
  module Resolver = Resolver.Make (IO)

  type 'f t =
    { name : string
    ; witness : 'f Service.scheme
    ; flow : Service.flow option }
  and 'f flow = (module Service.FLOW with type flow = 'f)

  let register ~name resolver service =
    let witness = Service.register resolver service in
    { name; witness; flow= None }

  let witness : type f. f t -> f Service.scheme = fun { witness; _ } -> witness
  let flow { flow; _ } = flow

  let bind : 'e t -> ('e -> 'e t) -> 'e t =
    fun t f ->
      let f e = match f e with { flow= Some flow; _ } -> flow | _ -> assert false in
      match Option.map (fun flow -> Service.bind t.witness flow f) t.flow with
      | Some flow -> { t with flow }
      | None -> t

  let map : 'a t -> ('a -> 'a) -> 'a t =
    fun t f ->
      match Option.map (fun flow -> Service.map t.witness t.witness flow f) t.flow with
      | Some flow -> { t with flow }
      | None -> assert false (* impossible case *)

  let extract : 'f t -> ('f -> 'f flow -> 'a IO.t) -> ('a IO.t, [ `Msg of string]) result =
    fun t f -> match t.flow with
      | Some flow -> Service.extract t.witness flow f
      | None ->
        Fmt.invalid_arg "Kind of flow %s was not initialized \
                         (you should resolve something before extracting something)"
          t.name
  (* [resolve] needs to be called at first. *)

  let resolve : Domain_name.t -> Resolver.t -> 'e t -> ('e t, [ `Unresolved | `Msg of string ]) result IO.t =
    fun domain resolver t ->
      IO.bind (Service.resolve domain resolver t.witness) @@ function
      | Ok flow -> IO.return (Ok { t with flow= Some flow })
      | Error _ as err -> IO.return err
end
