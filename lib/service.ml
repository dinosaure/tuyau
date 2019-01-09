module type SERVICE = sig
  type description
  type endpoint
  type from

  type buffer
  type +'a io

  val init : description -> from -> endpoint -> endpoint io
  val read : endpoint -> buffer -> int io
  val write : endpoint -> buffer -> int io
end

module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) = struct
  module Resolver = Resolver.Make(IO)

  type kind = UDP | TCP

  type desc =
    { name : string
    ; port : int
    ; kind : kind }

  type ('r, 'e) init = desc -> 'r -> 'e -> 'e IO.t
  type 'e read = 'e -> B.t -> int IO.t
  type 'e write = 'e -> B.t -> int IO.t

  type ('r, 'e) service =
    { desc : desc
    ; init : ('r, 'e) init
    ; rd : 'e read
    ; wr : 'e write }

  type 'e action =
    { rd : 'e -> B.t -> int IO.t
    ; wr : 'e -> B.t -> int IO.t }

  module type SERVICE = SERVICE with type description = desc and type 'a io = 'a IO.t and type buffer = B.t

  let of_module (type e r) ~name ~port ~kind
    (module Service : SERVICE with type endpoint = e
                               and type from = r)
    =
    { desc = { name; port; kind; }
    ; init = Service.init
    ; rd = Service.read
    ; wr = Service.write }

  module Service = struct type 'e t = B : 'r Resolver.resolver * ('r, 'e) service -> 'e t end

  open E0

  module Dispatch = Make(Service)

  type 'e scheme = 'e Dispatch.extension

  let add
    : type r e. r Resolver.resolver -> (r, e) service -> e scheme
    = fun resolver service ->
      let value = Service.B (resolver, service) in
      Dispatch.inj value

  type endpoint = Dispatch.t

  let endpoint : type e. e scheme -> e -> endpoint =
    fun scheme endpoint ->
      let module Scheme = (val scheme) in
      Scheme.T endpoint

  let resolve
    : type e. Domain_name.t -> Resolver.t -> e scheme -> endpoint -> endpoint option IO.t
    = fun domain m scheme endpoint ->
      match Dispatch.extract endpoint scheme with
      | None -> IO.return None
      | Some e ->
        let module Scheme = (val scheme) in
        let binding = Scheme.instance in
        let Service.B (resolver, service) = binding in
        IO.bind (Resolver.resolve domain resolver m) @@ function
        | None -> IO.return None
        | Some v ->
          IO.map (fun e -> Some (Scheme.T e)) (service.init service.desc v e)

  let instance : type e a. e scheme -> (e action -> a) -> a
    = fun scheme f ->
      let module Scheme = (val scheme) in
      let Service.B (_, service) = Scheme.instance in
      f { rd= service.rd
        ; wr= service.wr }

  let bind : type e. e scheme -> endpoint -> (e -> endpoint) -> endpoint option
    = fun scheme endpoint f -> match Dispatch.extract endpoint scheme with
      | Some e -> Some (f e)
      | None -> None

  let return = endpoint

  let map : type a b. a scheme -> b scheme -> endpoint -> (a -> b) -> endpoint option
    = fun sx sy endpoint f -> match Dispatch.extract endpoint sx with
      | Some e ->
        let module Scheme = (val sy) in
        let e = f e in
        Some (Scheme.T e)
      | None -> None
end
