type endp =
  [ `TCP of Ipaddr.t * int
  | `Unix_domain_socket of string
  | `Vchan_direct of int * string
  | `Vchan_domain_socket of string * string
  | `TLS of string * endp
  | `Unknown of string ]

module type S = sig
  module IO : Sigs.IO
  module B : Sigs.SINGLETON

  module High : module type of High.Make (IO) (B)

  type t
  type svc = Service.service
  type value = S : 'f High.t -> value

  type services = Scheme.t -> value option IO.t
  type +'a io = 'a IO.t

  val register_scheme : Scheme.t -> 'v High.t -> unit
  val services : t -> services

  module Resolver : sig
    type t = High.Resolver.t
    type resolver = endp High.Resolver.resolver

    val make : name:string -> resolver
    val name : resolver -> string
    val empty : t
    val add : resolver -> resolve:(Domain_name.t -> endp option IO.t) -> t -> t
    val rem : resolver -> t -> t
    val get : resolver -> t -> (Domain_name.t -> endp option IO.t)
    val resolve : Domain_name.t -> resolver -> t -> endp option IO.t
    val default : resolver
  end

  type error = [ `Invalid_uri | `Msg of string | `Not_found | `Unresolved ]

  val make : ?services:services -> unit -> t
  val resolve : ?resolver:Resolver.t -> uri:Uri.t -> t -> (value * High.Service.flow, error) result io
end

module Make (IO : Sigs.IO) (B : Sigs.SINGLETON)
  : S with module IO = IO and module B = B
= struct
  module IO = IO
  module B = B
  module High = High.Make (IO) (B)

  type value = S : 'f High.t -> value

  let schemes : (Scheme.t, value) Hashtbl.t = Hashtbl.create 0x10

  let register_scheme
    : type v. Scheme.t -> v High.t -> unit
    = fun k v -> Hashtbl.add schemes k (S v)

  let service_fn_from_ex_nihilo scheme =
    match Hashtbl.find schemes scheme with
      | S high -> IO.return (Some (S high))
      | exception Not_found -> IO.return None

  type svc = Service.service
  type services = Scheme.t -> value option IO.t
  type +'a io = 'a IO.t

  let default_services = service_fn_from_ex_nihilo

  type t =
    { resolvers : High.Resolver.t
    ; services : services }

  let make ?(services= default_services) () =
    { resolvers= High.Resolver.empty
    ; services }

  let services t = t.services

  (* XXX(dinosaure): Specialized form of [Resolver] to provide __only__ [endp]. *)

  module Resolver : sig
    type t = High.Resolver.t

    type resolver = endp High.Resolver.resolver

    val make : name:string -> resolver
    val name : resolver -> string
    val empty : t
    val add : resolver -> resolve:(Domain_name.t -> endp option IO.t) -> t -> t
    val rem : resolver -> t -> t
    val get : resolver -> t -> (Domain_name.t -> endp option IO.t)
    val resolve : Domain_name.t -> resolver -> t -> endp option IO.t
    val default : resolver
  end = struct
    include (High.Resolver : (module type of Resolver.Make (IO)) with type 'a resolver := 'a High.Resolver.resolver
            and type t := High.Resolver.t)

    type t = High.Resolver.t
    type resolver = endp High.Resolver.resolver

    let default : resolver = make ~name:"fucking endp"
  end

  type error = [ `Invalid_uri | `Msg of string | `Not_found | `Unresolved ]

  let resolve
    : ?resolver:Resolver.t -> uri:Uri.t -> t -> (value * High.Service.flow, error) result io
    = fun ?resolver ~uri t ->
      match Option.map Scheme.of_string (Uri.scheme uri) with
      | None -> IO.return (Error `Invalid_uri)
      | Some (Error _ as err) -> IO.return err
      | Some (Ok scheme) ->
        let open IO in

        t.services scheme >>= function
        | None -> return (Error `Not_found)
        | Some (S high) ->
          let host = match Uri.host uri with
            | None -> "localhost" (* XXX(dinosaure): uh? *)
            | Some host -> host in
          let domain_name = Domain_name.of_string_exn host in
          let resolvers = Option.value ~default:t.resolvers (resolver :> High.Resolver.t option) in
          High.resolve domain_name resolvers high >>= function
          | Error _ as err -> IO.return (err :> (value * High.Service.flow, error) result)
          | Ok high -> match High.flow high with
            | Some flow -> IO.return (Ok (S high, flow))
            | None -> IO.return (Error `Unresolved) (* XXX(dinosaure): should never occur. *)
end
