[@@@warning "-32-34"]

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

type endp =
  [ `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
  | `Vchan_direct of int * string
  | `Vchan_domain_socket of string * string
  | `TLS of string * endp
  | `Unknown of string
  | `None ]

type service =
  { name : string
  ; port : int
  ; tls : bool }

module Option = struct
  type 'a t = 'a option

  let map f = function
    | Some x -> Some (f x)
    | None -> None
end

module type LikeConduit = functor (IO : Sigs.IO) -> functor (B : Sigs.SINGLETON) -> sig
  type t
  type svc = Service.service
  type rewrite_fn = svc -> Uri.t -> endp IO.t
  type service_fn = string -> svc option IO.t
  type +'a io = 'a IO.t

  val (++) : service_fn -> service_fn -> service_fn
  val init : ?service:service_fn -> ?rewrites:(string * rewrite_fn) list -> unit -> t
  val add_rewrite : host:string -> f:rewrite_fn -> t -> unit
  val set_service : f:service_fn -> t -> unit
  val service : t -> service_fn
  val resolve_uri : ?rewrites:(string * rewrite_fn) list -> uri:Uri.t -> t -> endp io
end
