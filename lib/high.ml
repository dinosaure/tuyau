[@@@warning "-32-34"]

module Option = struct
  type 'a t = 'a option

  let map f = function
    | Some x -> Some (f x)
    | None -> None
end

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

module type Like_conduit = functor (IO : Sigs.IO) -> functor (B : Sigs.SINGLETON) -> sig
  type t
  type svc = Service.service
  type rewrite_fn = svc -> Uri.t -> endp IO.t
  (* type service_fn = string -> svc option IO.t *)
  type +'a io = 'a IO.t

  (*
  val (++) : service_fn -> service_fn -> service_fn
  val init : ?service:service_fn -> ?rewrites:(string * rewrite_fn) list -> unit -> t
  val add_rewrite : host:string -> f:rewrite_fn -> t -> unit
  val set_service : f:service_fn -> t -> unit
  val service : t -> service_fn
  *)

  type error = [ `Invalid_uri | `Msg of string | `Not_found | `Unresolved ]

  val resolve_uri : ?rewrites:(string * rewrite_fn) list -> uri:Uri.t -> t -> (endp, error) result io
end

module Scheme : sig
  type t = private string

  val of_string : string -> (t, [ `Msg of string ]) result
  val of_string_exn : string -> t
  val to_string : t -> string
end = struct
  type t = string

  let of_string x = Ok x
  let of_string_exn x = match of_string x with
    | Ok v -> v
    | Error (`Msg err) -> invalid_arg err
  let to_string x = x
end

module Conduit : Like_conduit = functor (IO : Sigs.IO) -> functor (B : Sigs.SINGLETON) -> struct
  module High = Make (IO) (B)

  type value = S : endp High.t -> value

  let schemes : (Scheme.t, value) Hashtbl.t = Hashtbl.create 0x10

  let register_scheme
    : Scheme.t -> endp High.t -> unit
    = fun k v -> Hashtbl.add schemes k (S v)

  let service_fn_from_ex_nihilo unsafe_scheme =
    match Scheme.of_string unsafe_scheme with
    | Error _ -> IO.return None
    | Ok v -> match Hashtbl.find schemes v with
      | S high ->
        let desc = High.Service.service_of_scheme high.witness in
        IO.return (Some desc)
      | exception Not_found -> IO.return None

  type svc = Service.service
  type rewrite_fn = svc -> Uri.t -> endp IO.t
  type service_fn = Scheme.t -> value option IO.t
  type +'a io = 'a IO.t

  let default_service = service_fn_from_ex_nihilo
  let default_lookup = High.Resolver.empty

  type t =
    { default_lookup : rewrite_fn
    ; mutable domains : High.Resolver.t
    ; mutable service : service_fn }

  let service t = t.service

  type error = [ `Invalid_uri | `Msg of string | `Not_found | `Unresolved ]

  let resolve_uri
    : ?rewrites:(string * rewrite_fn) list -> uri:Uri.t -> t -> (endp, error) result io
    = fun ?rewrites:_ ~uri t ->
      match Uri.scheme uri with
      | None -> IO.return (Error `Invalid_uri)
      | Some unsafe_scheme ->
        let open IO in

        t.service (Scheme.of_string_exn unsafe_scheme) >>= function
        | None -> return (Error `Not_found)
        | Some (S high) (* description *) ->
          (* XXX(dinosaure): a witness of a service exists somewhere. *)
          let host = match Uri.host uri with
            | None -> "localhost" (* XXX(dinosaure): uh? *)
            | Some host -> host in
          let domain_name = Domain_name.of_string_exn host in
          High.resolve domain_name t.domains high >>= function
          | Error _ as err -> IO.return (err :> (endp, error) result)
          | Ok high ->
            let extract flow = High.Service.extract high.High.witness flow (fun endp _ -> endp) in
            match Option.map extract high.High.flow with
            | Some (Ok endp) -> IO.return (Ok endp)
            | Some (Error _ as err) -> IO.return (err :> (endp, error) result)
            (* XXX(dinosaure): if we have the [None] case, it's mostly because
               [High.resolve] did set [t.flow] to [Some _]. So it's probably
               because we did not resolve [domain_name]. *)
            | None -> IO.return (Error `Unresolved)
end
