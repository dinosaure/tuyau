type kind =
  | UDP | TCP

type service =
  { name : string
  ; port : int
  ; kind : kind }

module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) = struct
  module Resolver = Resolver.Make(IO)

  type nonrec kind = kind = UDP | TCP

  type desc = service =
    { name : string
    ; port : int
    ; kind : kind }

  module type SERVICE = Sigs.SERVICE
    with type description = desc
     and type 'a io = 'a IO.t
     and type buffer = B.t
  module type FLOW = Sigs.FLOW
    with type 'a io = 'a IO.t
     and type buffer = B.t

  type ('endpoint, 'flow) service =
    { desc : desc
    ; implementation : (module SERVICE with type endpoint = 'endpoint and type flow = 'flow) }

  let of_module (type e f) ~name ~port ~kind
    (module Service : SERVICE with type endpoint = e
                               and type flow = f)
    =
    { desc = { name; port; kind; }
    ; implementation = (module Service) }

  open E0

  module Binding = struct type 'f t = B : 'e Resolver.resolver * ('e, 'f) service -> 'f t end
  module Dispatch = Make(Binding)

  type 'f scheme = 'f Dispatch.extension

  let register
    : type e f. e Resolver.resolver -> (e, f) service -> f scheme
    = fun resolver service ->
      let value = Binding.B (resolver, service) in
      Dispatch.inj value

  type flow = Dispatch.t

  let flow : type f. f scheme -> f -> flow =
    fun scheme flow ->
      let module Scheme = (val scheme) in
      Scheme.T flow

  let resolve
    : type f. Domain_name.t -> Resolver.t -> f scheme -> flow -> (flow, [ `Unresolved | `Msg of string ]) result IO.t
    = fun domain m scheme flow ->
      match Dispatch.extract flow scheme with
      | None ->
        let module Scheme = (val scheme) in
        let Binding.B (_, service0) = Scheme.instance in
        let Dispatch.V (_, binding1) = Dispatch.prj flow in
        let Binding.B (_, service1) = binding1 in
        let err = Rresult.R.error_msgf "Invalid extraction from scheme %s and flow %s.\n%!"
          service0.desc.name
          service1.desc.name in
        IO.return err
      | Some flow ->
        let module Scheme = (val scheme) in
        let binding = Scheme.instance in
        let Binding.B (resolver, service) = binding in
        IO.bind (Resolver.resolve domain resolver m) @@ function
        | None -> IO.return (Rresult.R.error `Unresolved)
        | Some v ->
          let module Service = (val service.implementation) in
          IO.map (function
              | Ok flow -> Ok (Scheme.T flow)
              | Error err -> Rresult.R.error_msgf "Initialization error: %a" Service.pp_error err)
            (Service.init service.desc v flow)

  let extract : type f a. f scheme -> flow -> (f -> (module FLOW with type flow = f) -> a) -> (a, [ `Msg of string ]) result
    = fun scheme flow f ->
      let module Scheme = (val scheme) in
      let Binding.B (_, service) = Scheme.instance in
      let module Service = (val service.implementation) in
      match Dispatch.extract flow scheme with
      | Some flow -> Ok (f flow (module Service : FLOW with type flow = f))
      | None ->
        let Binding.B (_, service0) = Scheme.instance in
        let Dispatch.V (_, binding1) = Dispatch.prj flow in
        let Binding.B (_, service1) = binding1 in
        Rresult.R.error_msgf "Invalid extraction from scheme %s and flow %s.\n%!"
          service0.desc.name
          service1.desc.name

  let bind : type f. f scheme -> flow -> (f -> flow) -> flow option
    = fun scheme flow f -> match Dispatch.extract flow scheme with
      | Some flow -> Some (f flow)
      | None -> None

  let return = flow

  let map : type a b. a scheme -> b scheme -> flow -> (a -> b) -> flow option
    = fun sx sy flow f -> match Dispatch.extract flow sx with
      | Some flow  ->
        let module Scheme = (val sy) in
        let flow = f flow in
        Some (Scheme.T flow)
      | None -> None
end
