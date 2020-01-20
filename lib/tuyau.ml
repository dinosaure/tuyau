module Sigs = Sigs

module type S = sig
  type input
  type output

  type +'a s

  module type S = Sigs.S
    with type +'a s = 'a s

  module type F = Sigs.F
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type ('edn, 't, 'flow) service =
    (module S with type endpoint = 'edn
               and type t = 't
               and type flow = 'flow)
  type ('edn, 'flow) protocol =
    (module F with type endpoint = 'edn
               and type flow = 'flow)

  module type FLOW = Sigs.FLOW
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type flow = Flow : 'flow * (module FLOW with type flow = 'flow) -> flow

  module type RESOLVER = Sigs.RESOLVER
    with type +'a s = 'a s

  type 'edn resolver = [ `host ] Domain_name.t -> ('edn option) s
  type 'edn key

  module Witness : sig
    type 'flow protocol
    type 't service
  end

  module Map : sig
    type t

    val empty : t
    val is_empty : t -> bool

    val mem : 'a key -> t -> bool
    val rem : 'a key -> t -> t
    val len : t -> int
  end

  val key : name:string -> 'edn key

  val register_service : key:'edn key -> service:('edn, 't, 'flow) service -> protocol:'flow Witness.protocol -> ('t * 'flow) Witness.service
  val register_protocol : key:'edn key -> protocol:('edn, 'flow) protocol -> 'flow Witness.protocol
  val register_resolver : key:'edn key -> 'edn resolver -> Map.t -> Map.t

  type error = [ `Msg of string | `Not_found | `Unresolved | `Invalid_key ]

  val pp_error : error Fmt.t

  type p

  val lift : 'flow -> 'flow Witness.protocol -> (p, [> error ]) result s
  val unlift : p -> flow

  val flow_of_endpoint : key:'edn key -> 'edn -> (p, [> error ]) result s
  val flow_of_protocol : key:'edn key -> 'edn -> protocol:'flow Witness.protocol -> ('flow, [> error ]) result s
  val flow : Map.t -> ?key:'edn key -> ?protocol:'flow Witness.protocol -> [ `host ] Domain_name.t -> (p, [> error ]) result s

  val service : key:'edn key -> 'edn -> service:('t * 'flow) Witness.service -> ('t * 'flow Witness.protocol, [> error ]) result s
  val server : key:'edn key -> ('t * 'flow) Witness.service -> ((module S with type endpoint = 'edn and type t = 't and type flow = 'flow), [> error ]) result
  val protocol : key:'edn key -> 'flow Witness.protocol -> ((module F with type endpoint = 'edn and type flow = 'flow), [> error ]) result
end

module Make
    (Scheduler : Sigs.SCHEDULER)
    (Input : Sigs.SINGLETON)
    (Output : Sigs.SINGLETON)
  : S with type input = Input.t
       and type output = Output.t
       and type +'a s = 'a Scheduler.t
= struct
  type input = Input.t
  type output = Output.t

  type +'a s = 'a Scheduler.t

  module type S = Sigs.S
    with type +'a s = 'a s

  module type F = Sigs.F
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type ('edn, 't, 'flow) service =
    (module S with type endpoint = 'edn
               and type t = 't
               and type flow = 'flow)
  type ('edn, 'flow) protocol =
    (module F with type endpoint = 'edn
               and type flow = 'flow)

  module type FLOW = Sigs.FLOW
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type flow =
    Flow : 'flow * (module FLOW with type flow = 'flow) -> flow

  module type RESOLVER = Sigs.RESOLVER
    with type +'a s = 'a s

  module Rs = E1.Make
      (struct type 'edn t = string end)
      (struct type 'edn t = (module RESOLVER with type endpoint = 'edn) end)

  module B = struct type 't t = Protocol : 'edn Rs.key * ('edn, 'flow) protocol -> 'flow t end
  module Pt = E0.Make (B)
  module A = struct type 't t = Service : 'edn Rs.key * ('edn, 't, 'flow) service * 'flow Pt.s -> ('t * 'flow) t end
  module Ss = E0.Make (A)


  type 'edn key = 'edn Rs.key
  type 'edn resolver = [ `host ] Domain_name.t -> 'edn option s

  module Witness = struct
    type 't service = 't Ss.s
    type 'flow protocol = 'flow Pt.s
  end

  module Map = struct
    type t = Rs.t

    let empty = Rs.empty
    let is_empty = Rs.is_empty
    let mem = Rs.mem
    let rem = Rs.rem
    let len = Rs.len
  end

  let return = Scheduler.return
  let ( >>= ) x f = Scheduler.bind x f
  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> return (Error err)

  let key ~name = Rs.Key.create name

  let register_service
    : type edn t flow.
      key:edn key -> service:(edn, t, flow) service -> protocol:flow Witness.protocol -> (t * flow) Witness.service
    = fun ~key ~service ~protocol ->
      Ss.inj (Service (key, service, protocol))

  let register_protocol
    : type edn flow.
      key:edn key -> protocol:(edn, flow) protocol -> flow Witness.protocol
    = fun ~key ~protocol ->
      Pt.inj (Protocol (key, protocol))

  let register_resolver
    : type edn.
      key:edn key -> edn resolver -> Map.t -> Map.t
    = fun ~key resolve ->
      let module Resolve = struct
        type endpoint = edn
        type nonrec +'a s = 'a s

        let resolve = resolve
      end in
      Rs.add key (module Resolve)

  type error = [ `Msg of string | `Not_found | `Unresolved | `Invalid_key ]

  let pp_error ppf = function
    | `Msg err -> Fmt.string ppf err
    | `Not_found -> Fmt.string ppf "Not found"
    | `Unresolved -> Fmt.string ppf "Unresolved"
    | `Invalid_key -> Fmt.string ppf "Invalid key"

  type p = Pt.t

  let flow_of_endpoint
    : type edn. key:edn key -> edn -> (p, [> error ]) result s
    = fun ~key edn ->
      let rec go = function
        | [] -> return (Error `Not_found)
        | Pt.Key (ctor, Protocol (k, (module Protocol))) :: r ->
          match Rs.Key.(key == k) with
          | None -> go r
          | Some E1.Refl.Refl ->
            Protocol.flow edn >>= function
            | Ok flow -> return (Ok (ctor flow))
            | Error _err -> go r in
      go (Pt.bindings ())

  let flow_of_protocol
    : type edn flow. key:edn key -> edn -> protocol:flow Witness.protocol -> (flow, [> error ]) result s
    = fun ~key edn ~protocol:(module P) ->
      let Protocol (k', (module Protocol)) = P.witness in
      match Rs.Key.(key == k') with
      | None -> return (Error `Invalid_key)
      | Some E1.Refl.Refl ->
        Protocol.flow edn >>= function
        | Ok flow -> return (Ok flow)
        | Error err -> return (Rresult.R.error_msgf "%a" Protocol.pp_error err)

  type endpoint = Endpoint : 'edn key * 'edn -> endpoint

  let resolve
    : Map.t -> [ `host ] Domain_name.t -> endpoint list s
    = fun m domain_name ->
      let rec go acc = function
        | [] -> return acc
        | Rs.Value (k, (module Resolve)) :: r ->
          Resolve.resolve domain_name >>= function
          | Some edn -> go (Endpoint (k, edn) :: acc) r
          | None -> go acc r in
      go [] (Rs.bindings m)

  let create
    : Map.t -> [ `host ] Domain_name.t -> (p, [> error ]) result s
    = fun m domain_name ->
      resolve m domain_name >>= fun l ->
      let rec go = function
        | [] -> return (Error `Not_found)
        | Endpoint (key, edn) :: r ->
          flow_of_endpoint ~key edn >>= function
          | Ok flow -> return (Ok flow)
          | Error _err -> go r in
      go l

  let lift
    : type flow. flow -> flow Witness.protocol -> (p, [> error ]) result s
    = fun flow (module P) ->
      return (Ok (P.T flow))

  let unlift
    : p -> flow
    = fun w ->
      let Pt.Value (flow, Protocol (_, (module Protocol))) = Pt.prj w in
      Flow (flow, (module Protocol))

  let flow
    : type edn flow. Map.t -> ?key:edn key -> ?protocol:flow Witness.protocol -> [ `host ] Domain_name.t -> (p, [> error ]) result s
    = fun m ?key ?protocol domain_name ->
      match key, protocol with
      | None, None -> create m domain_name
      | Some key, None ->
        ( match Rs.find key m with
          | None -> return (Error `Not_found)
          | Some (module Resolve) ->
            Resolve.resolve domain_name >>= function
            | Some edn -> flow_of_endpoint ~key edn
            | None -> return (Error `Unresolved) )
      | None, Some protocol ->
        resolve m domain_name >>= fun l ->
        let rec go = function
          | [] -> return (Error `Not_found)
          | Endpoint (key, edn) :: r ->
            flow_of_protocol ~key edn ~protocol >>= function
            | Ok flow -> lift flow protocol
            | Error _err -> go r in
        go l
      | Some key, Some protocol ->
        match Rs.find key m with
        | None -> return (Error `Not_found)
        | Some (module Resolve) ->
          Resolve.resolve domain_name >>= function
          | Some edn -> flow_of_protocol ~key edn ~protocol >>? fun flow -> lift flow protocol
          | None -> return (Error `Unresolved)

  let service
    : type edn t flow. key:edn key -> edn -> service:(t * flow) Witness.service -> (t * flow Witness.protocol, [> error ]) result s
    = fun ~key edn ~service:(module S) ->
      let Service (k', (module Service), protocol) = S.witness in
      match Rs.Key.(key == k') with
      | None -> return (Error `Invalid_key)
      | Some E1.Refl.Refl ->
        Service.make edn >>= function
        | Ok t ->
          return (Ok (t, protocol))
        | Error err ->
          return (Rresult.R.error_msgf "%a" Service.pp_error err)

  let server
    : type edn t flow. key:edn key -> (t * flow) Witness.service -> ((module S with type endpoint = edn and type t = t and type flow = flow), [> error ]) result
    = fun ~key (module S) ->
      let Service (k, (module Service), _) = S.witness in
      match Rs.Key.(key == k) with
      | Some E1.Refl.Refl -> Ok (module Service)
      | None -> Error `Invalid_key

  let protocol
    : type edn flow. key:edn key -> flow Witness.protocol -> ((module F with type endpoint = edn and type flow = flow), [> error ]) result
    = fun ~key (module P) ->
      let Protocol (k, (module Protocol)) = P.witness in
      match Rs.Key.(key == k) with
      | Some E1.Refl.Refl -> Ok (module Protocol)
      | None -> Error `Invalid_key
end
