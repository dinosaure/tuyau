module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) : sig
  module Resolver : module type of Resolver.Make(IO)

  type kind = UDP | TCP
  (** Kind of ground protocol. *)

  type desc =
    { name : string
    ; port : int
    ; kind : kind }
  (** Description of a service. *)

  module type SERVICE = Sigs.SERVICE
    with type description = desc
     and type 'a io = 'a IO.t
     and type buffer = B.t
  (** Specialized signature which a service implementation must respect. *)

  module type FLOW = Sigs.FLOW
    with type 'a io = 'a IO.t
     and type buffer = B.t
  (** Specialized signature of a flow. Subset of {!SERVICE}. *)

  type ('e, 'f) service =
    { desc : desc
    ; implementation : (module SERVICE with type endpoint = 'e and type flow = 'f) }
  (** Packed value of a service implementation binded with a description. *)

  val of_module : name:string -> port:int -> kind:kind ->
    (module SERVICE with type endpoint = 'e
                     and type flow = 'f)
    -> ('e, 'f) service
  (** [of_module ~name ~port ~kind (module Service)] makes a {!service} from an
     implementation and a description. *)

  type 'f scheme
  (** A witness of a scheme. *)

  type flow
  (** Abstract type of a flow implementation. *)

  val register : 'e Resolver.resolver -> ('e, 'f) service -> 'f scheme
  (** [register key service] returns a witness which is an association between
     the resolver [key] and the service [service]. The association is only on
     the type-system. Then, binded implementation of [key] will be done the
     resolution and give the result to the service implementation. *)

  val flow : 'f scheme -> 'f -> flow
  (** [flow witness flow] hides a physical value of a flow represented by
     [witness] to abstract type {!flow}. *)

  val resolve : Domain_name.t -> Resolver.t -> 'f scheme -> flow -> (flow, [ `Unresolved | `Msg of string ]) result IO.t
  (** [resolve domain resolvers witness flow] tries to resolve [domain] with
     binded resolver to [witness] (see {!register}) available in [resolvers].
     Then, iff binded resolver find a solution, {!SERVICE.init} is called with
     unwrapped/unhidden [flow] with solution. *)

  val extract : 'f scheme -> flow -> ('f -> (module FLOW with type flow = 'f) -> 'a) -> ('a, [ `Msg of string ]) result
  val bind : 'f scheme -> flow -> ('f -> flow) -> flow option
  val map : 'a scheme -> 'b scheme -> flow -> ('a -> 'b) -> flow option
  val return : 'f scheme -> 'f -> flow
end
