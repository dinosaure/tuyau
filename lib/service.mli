module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) : sig
  module Resolver : module type of Resolver.Make(IO)

  type kind = UDP | TCP

  type desc =
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

  type ('e, 'f) service =
    { desc : desc
    ; implementation : (module SERVICE with type endpoint = 'e and type flow = 'f) }

  val of_module : name:string -> port:int -> kind:kind ->
    (module SERVICE with type endpoint = 'e
                     and type flow = 'f)
    -> ('e, 'f) service

  type 'f scheme

  type flow

  val register : 'e Resolver.resolver -> ('e, 'f) service -> 'f scheme
  val flow : 'f scheme -> 'f -> flow
  val resolve : Domain_name.t -> Resolver.t -> 'f scheme -> flow -> (flow, [ `Unresolved | `Msg of string ]) result IO.t

  val extract : 'f scheme -> flow -> ('f -> (module FLOW with type flow = 'f) -> 'a) -> ('a, [ `Msg of string ]) result
  val bind : 'f scheme -> flow -> ('f -> flow) -> flow option
  val map : 'a scheme -> 'b scheme -> flow -> ('a -> 'b) -> flow option
  val return : 'f scheme -> 'f -> flow
end
