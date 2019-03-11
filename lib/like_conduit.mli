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

module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) : S with module IO = IO and module B = B
