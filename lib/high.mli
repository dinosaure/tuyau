module Make (IO : Sigs.IO) (B : Sigs.SINGLETON): sig
  module Service : module type of Service.Make (IO) (B)
  module Resolver : module type of Service.Resolver

  type 'f t
  type 'f flow = (module Service.FLOW with type flow = 'f)

  val register : name:string -> 'e Resolver.resolver -> ('e, 'f) Service.service -> 'f t
  val bind : 'f t -> ('f -> 'f t) -> 'f t
  val map : 'f t -> ('f -> 'f) -> 'f t
  val extract : 'f t -> ('f -> 'f flow -> 'a IO.t) -> ('a IO.t, [ `Msg of string ]) result
  val resolve : Domain_name.t -> Resolver.t -> 'f t -> ('f t, [ `Unresolved | `Msg of string]) result IO.t
end
