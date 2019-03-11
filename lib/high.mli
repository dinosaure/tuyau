module Make (IO : Sigs.IO) (B : Sigs.SINGLETON): sig
  module Service : module type of Service.Make (IO) (B)
  module Resolver : module type of Service.Resolver

  type 'f t
  (** Type of witness of service implementation. *)

  type 'f flow = (module Service.FLOW with type flow = 'f)
  (** Type of service implementation. *)

  val register : name:string -> 'e Resolver.resolver -> ('e, 'f) Service.service -> 'f t
  (** [register ~name resolver service] makes a new ['f t] which is not
     initialized. It binds key [resolver] with [service]. On the resolution
     process, we will take implementation binded to [resolver] to resolve
     domain-name. Abstracted value returned will be send to the
     {!Sigs.SERVICE.make} implementation. *)

  val extract : 'f t -> ('f -> 'f flow -> 'a IO.t) -> ('a IO.t, [ `Msg of string ]) result
  (** [extract t f] extracts real-value of [t] and apply [f] with it. [f] is
     called if if [t] is initialized (see {!resolve}). Otherwise, it returns an
     error. *)

  val resolve : Domain_name.t -> Resolver.t -> 'f t -> ('f t, [ `Unresolved | `Msg of string]) result IO.t
  (** [resolve domain resolvers witness] tries to resolve [domain] and send
     results to the {!Sigs.SERVICE.make} function binded with [witness]. It
     returns an abstraction of the (hidden) value returned. *)

  val witness : 'f t -> 'f Service.scheme
  val flow : 'f t -> Service.flow option

  (** Convenience operators. *)

  val bind : 'f t -> ('f -> 'f t) -> 'f t
  val map : 'f t -> ('f -> 'f) -> 'f t
end
