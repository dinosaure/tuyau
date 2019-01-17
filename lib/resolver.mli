module Make (IO : Sigs.IO) : sig
  type t

  module Identifier : sig
    type t = private int

    val compare : t -> t -> int
    val equal : t -> t -> bool
  end

  type 'v resolver

  val make : name:string -> 'v resolver
  val name : 'v resolver -> string
  val identifier : 'v resolver -> Identifier.t

  val empty : t
  val add : 'v resolver -> resolve:(Identifier.t list -> Domain_name.t -> 'v option IO.t) -> t -> t
  val rem : 'v resolver -> t -> t
  val get : 'v resolver -> t -> (Identifier.t list -> Domain_name.t -> 'v option IO.t)

  val resolve : ?colored:Identifier.t list -> Domain_name.t -> 'v resolver -> t -> 'v option IO.t
end
