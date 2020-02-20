module Sigs = Sigs

type _ witness = ..
type _ resolver =
  | Resolver : { priority : int
               ; resolve : ([ `host ] Domain_name.t -> ('edn option, 's) Sigs.app)
               ; witness : 's witness } -> ('edn * 's) resolver

module Map = E1.Make
    (struct type _ t = string end)
    (struct type 'a t = 'a resolver end)

type resolvers = Map.t
type 'a key = 'a Map.key

let empty = Map.empty

module type S = sig
  type input
  type output

  type +'a s
  type scheduler

  module type PROTOCOL = Sigs.PROTOCOL
    with type input = input
     and type output = output
     and type +'a s = 'a s

  module type SERVICE = Sigs.SERVICE
    with type +'a s = 'a s

  type ('edn, 't, 'flow) service =
    (module SERVICE with type endpoint = 'edn
                     and type t = 't
                     and type flow = 'flow)

  type ('edn, 'flow) protocol =
    (module PROTOCOL with type endpoint = 'edn
                          and type flow = 'flow)

  module type FLOW = Sigs.FLOW
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type flow = Flow : 'flow * (module FLOW with type flow = 'flow) -> flow
  type 'edn resolver = [ `host ] Domain_name.t -> ('edn option) s

  type 'edn key = ('edn * scheduler) Map.key

  module Witness : sig
    type 'flow protocol
    type 't service
  end

  val key : string -> 'edn key
  val name_of_key : 'edn key -> string

  val register_service
    :  key:'edn key
    -> service:('edn, 't, 'flow) service
    -> protocol:'flow Witness.protocol
    -> ('t * 'flow) Witness.service

  val register_protocol
    :  key:'edn key
    -> protocol:('edn, 'flow) protocol
    -> 'flow Witness.protocol

  val register_resolver
    :  key:'edn key
    -> ?priority:int
    -> 'edn resolver
    -> resolvers
    -> resolvers

  type error = [ `Msg of string | `Not_found | `Unresolved | `Invalid_key ]

  val pp_error : error Fmt.t

  val abstract : 'flow Witness.protocol -> 'flow -> flow

  val flow_of_endpoint
    :  key:'edn key
    -> 'edn
    -> (flow, [> error ]) result s

  val flow_of_protocol
    :  key:'edn key
    -> 'edn
    -> protocol:'flow Witness.protocol
    -> ('flow, [> error ]) result s

  val flow
    :  resolvers
    -> ?key:'edn key
    -> ?protocol:'flow Witness.protocol
    -> [ `host ] Domain_name.t
    -> (flow, [> error ]) result s

  val serve
    :  key:'edn key
    -> 'edn
    -> service:('t * 'flow) Witness.service
    -> ('t * 'flow Witness.protocol, [> error ]) result s

  val impl_of_service
    :  key:'edn key
    -> ('t * 'flow) Witness.service
    -> ((module SERVICE with type endpoint = 'edn
                         and type t = 't
                         and type flow = 'flow),
        [> error ]) result

  val impl_of_protocol
    :  key:'edn key
    -> 'flow Witness.protocol
    -> ((module PROTOCOL with type endpoint = 'edn
                              and type flow = 'flow),
        [> error ]) result

  val impl_of_flow : 'flow Witness.protocol -> (module FLOW with type flow = 'flow)
end

module Make
    (Scheduler : Sigs.SCHEDULER)
    (Input : Sigs.SINGLETON)
    (Output : Sigs.SINGLETON)
  : S with type input = Input.t
       and type output = Output.t
       and type +'a s = 'a Scheduler.t
= struct
  module Bijection = Sigs.Higher(Scheduler)

  let inj = Bijection.inj
  let prj = Bijection.prj

  type scheduler = Bijection.t
  type _ witness += Witness : scheduler witness

  let witness : scheduler witness = Witness

  type input = Input.t
  type output = Output.t

  type +'a s = 'a Scheduler.t

  module type PROTOCOL = Sigs.PROTOCOL
    with type input = input
     and type output = output
     and type +'a s = 'a s

  module type SERVICE = Sigs.SERVICE
    with type +'a s = 'a s

  type ('edn, 't, 'flow) service =
    (module SERVICE with type endpoint = 'edn
                     and type t = 't
                     and type flow = 'flow)

  type ('edn, 'flow) protocol =
    (module PROTOCOL with type endpoint = 'edn
                          and type flow = 'flow)

  module type FLOW = Sigs.FLOW
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type flow =
    Flow : 'flow * (module FLOW with type flow = 'flow) -> flow
  type 'edn key = ('edn * scheduler) Map.key
  type 'edn resolver = [ `host ] Domain_name.t -> 'edn option s

  module B = struct
    type 't t =
      Protocol : 'edn key * ('edn, 'flow) protocol -> 'flow t
  end
  module Ptr = E0.Make (B)
  module A = struct
    type 't t =
      Service : 'edn key * ('edn, 't, 'flow) service * 'flow Ptr.s -> ('t * 'flow) t
  end
  module Svc = E0.Make (A)

  module Witness = struct
    type 't service = 't Svc.s
    type 'flow protocol = 'flow Ptr.s
  end

  let return = Scheduler.return
  let ( >>= ) x f = Scheduler.bind x f
  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> return (Error err)

  let key name = Map.Key.create name
  let name_of_key : type edn. edn key -> string = fun key -> (Map.Key.info key)

  let register_service
    : type edn t flow.
         key:edn key
      -> service:(edn, t, flow) service
      -> protocol:flow Witness.protocol
      -> (t * flow) Witness.service
    = fun ~key ~service ~protocol ->
      Svc.inj (Service (key, service, protocol))

  let register_protocol
    : type edn flow.
      key:edn key -> protocol:(edn, flow) protocol -> flow Witness.protocol
    = fun ~key ~protocol ->
      Ptr.inj (Protocol (key, protocol))

  let ( <.> ) f g = fun x -> f (g x)

  let register_resolver
    : type edn.
      key:edn key -> ?priority:int -> edn resolver -> resolvers -> resolvers
    = fun ~key ?(priority= 0) resolve ->
      let resolve = inj <.> resolve in
      Map.add key (Resolver { priority; resolve; witness; })

  type error = [ `Msg of string | `Not_found | `Unresolved | `Invalid_key ]

  let pp_error ppf = function
    | `Msg err -> Fmt.string ppf err
    | `Not_found -> Fmt.string ppf "Not found"
    | `Unresolved -> Fmt.string ppf "Unresolved"
    | `Invalid_key -> Fmt.string ppf "Invalid key"

  let flow_of_endpoint
    : type edn.
         key:edn key
      -> edn
      -> (flow, [> error ]) result s
    = fun ~key edn ->
      let rec go = function
        | [] -> return (Error `Not_found)
        | Ptr.Key (Protocol (k, (module Protocol))) :: r ->
          match Map.Key.(key == k) with
          | None -> go r
          | Some E1.Refl.Refl ->
            Protocol.flow edn >>= function
            | Ok flow -> return (Ok (Flow (flow, (module Protocol))))
            | Error _err -> go r in
      go (Ptr.bindings ())

  let flow_of_protocol
    : type edn flow.
         key:edn key
      -> edn
      -> protocol:flow Witness.protocol
      -> (flow, [> error ]) result s
    = fun ~key edn ~protocol:(module P) ->
      let Protocol (k', (module Protocol)) = P.witness in
      match Map.Key.(key == k') with
      | None -> return (Error `Invalid_key)
      | Some E1.Refl.Refl ->
        Protocol.flow edn >>= function
        | Ok flow -> return (Ok flow)
        | Error err -> return (Rresult.R.error_msgf "%a" Protocol.pp_error err)

  type endpoint = Endpoint : 'edn key * 'edn -> endpoint

  module Refl = struct type ('a, 'b) t = Refl : ('a, 'a) t end
  let scheduler
    : type s. s witness -> (s, scheduler) Refl.t option
    = function
      | Witness -> Some Refl.Refl
      | _ -> None

  let resolve
    : resolvers -> [ `host ] Domain_name.t -> endpoint list s
    = fun m domain_name ->
      let rec go acc = function
        | [] -> return (List.rev acc) (* XXX(dinosaure): keep order. *)
        | Map.Value (k, Resolver { resolve; witness; _ }) :: r ->
          ( match scheduler witness with
            | None -> go acc r
            | Some Refl.Refl ->
              resolve domain_name |> prj >>= function
              | Some edn -> go (Endpoint (k, edn) :: acc) r
              | None -> go acc r ) in
      let compare
          (Map.Value (_, Resolver { priority= pa; _ }))
          (Map.Value (_, Resolver { priority= pb; _ })) =
        Stdlib.Int.compare pa pb in
      go [] (List.sort compare (Map.bindings m))

  let create
    : resolvers -> [ `host ] Domain_name.t -> (flow, [> error ]) result s
    = fun m domain_name ->
      resolve m domain_name >>= fun l ->
      let rec go = function
        | [] -> return (Error `Not_found)
        | Endpoint (key, edn) :: r ->
          flow_of_endpoint ~key edn >>= function
          | Ok flow -> return (Ok flow)
          | Error _err -> go r in
      go l

  let abstract
    : type v. v Witness.protocol -> v -> flow
    = fun (module P) flow ->
      let Ptr.Value (flow, Protocol (_, (module Protocol))) = Ptr.prj (P.T flow) in
      Flow (flow, (module Protocol))

  let flow
    : type edn f.
         resolvers
      -> ?key:edn key
      -> ?protocol:f Witness.protocol
      -> [ `host ] Domain_name.t
      -> (flow, [> error ]) result s
    = fun m ?key ?protocol domain_name ->
      match key, protocol with
      | None, None -> create m domain_name
      | Some key, None ->
        ( match Map.find key m with
          | None -> return (Error `Not_found)
          | Some (Resolver { resolve; witness; _ }) ->
            ( match scheduler witness with
              | None -> return (Error `Unresolved)
              | Some Refl.Refl ->
                resolve domain_name |> prj >>= function
                | Some edn -> flow_of_endpoint ~key edn
                | None -> return (Error `Unresolved) ) )
      | None, Some protocol ->
        resolve m domain_name >>= fun l ->
        let rec go = function
          | [] -> return (Error `Not_found)
          | Endpoint (key, edn) :: r ->
            flow_of_protocol ~key edn ~protocol >>= function
            | Ok flow ->
              let module P = (val protocol) in
              let Protocol (_, (module Protocol)) = P.witness in
              return (Ok (Flow (flow, (module Protocol))))
            | Error _err -> go r in
        go l
      | Some key, Some protocol ->
        match Map.find key m with
        | None -> return (Error `Not_found)
        | Some (Resolver { resolve; witness; _ }) ->
          match scheduler witness with
          | None -> return (Error `Unresolved)
          | Some Refl.Refl ->
            resolve domain_name |> prj >>= function
            | Some edn -> flow_of_protocol ~key edn ~protocol >>? fun flow ->
              let module P = (val protocol) in
              let Protocol (_, (module Protocol)) = P.witness in
              return (Ok (Flow (flow, (module Protocol))))
            | None -> return (Error `Unresolved)

  let serve
    : type edn t flow.
         key:edn key
      -> edn
      -> service:(t * flow) Witness.service
      -> (t * flow Witness.protocol, [> error ]) result s
    = fun ~key edn ~service:(module S) ->
      let Service (k', (module Service), protocol) = S.witness in
      match Map.Key.(key == k') with
      | None -> return (Error `Invalid_key)
      | Some E1.Refl.Refl ->
        Service.make edn >>= function
        | Ok t ->
          return (Ok (t, protocol))
        | Error err ->
          return (Rresult.R.error_msgf "%a" Service.pp_error err)

  let impl_of_service
    : type edn t flow.
         key:edn key
      -> (t * flow) Witness.service
      -> ((module SERVICE with type endpoint = edn
                           and type t = t
                           and type flow = flow),
          [> error ]) result
    = fun ~key (module S) ->
      let Service (k, (module Service), _) = S.witness in
      match Map.Key.(key == k) with
      | Some E1.Refl.Refl -> Ok (module Service)
      | None -> Error `Invalid_key

  let impl_of_protocol
    : type edn flow.
         key:edn key
      -> flow Witness.protocol
      -> ((module PROTOCOL with type endpoint = edn
                                and type flow = flow),
          [> error ]) result
    = fun ~key (module P) ->
      let Protocol (k, (module Protocol)) = P.witness in
      match Map.Key.(key == k) with
      | Some E1.Refl.Refl -> Ok (module Protocol)
      | None -> Error `Invalid_key

  let impl_of_flow
    : type flow. flow Witness.protocol -> (module FLOW with type flow = flow)
    = fun (module P) ->
      let Protocol (_, (module Protocol)) = P.witness in
      (module Protocol)
end
