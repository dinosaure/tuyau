module Make (IO : Sigs.IO) : sig
  type t
  type 'v resolver

  val make : name:string -> 'v resolver
  val name : 'v resolver -> string

  val empty : t
  val add : 'v resolver -> resolve:(Domain_name.t -> 'v option IO.t) -> t -> t
  val rem : 'v resolver -> t -> t
  val get : 'v resolver -> t -> (Domain_name.t -> 'v option IO.t)

  val resolve : Domain_name.t -> 'v resolver -> t -> 'v option IO.t
end
