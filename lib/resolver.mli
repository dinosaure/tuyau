module Make (IO : Sigs.IO) : sig
  type t
  (** A map of resolvers. *)

  module Identifier : sig
    type t = private int
    (** Uniq identifier of a resolver. *)

    val compare : t -> t -> int
    (** comparison function of {!identifier}. *)

    val equal : t -> t -> bool
    (** equal function of {!identifier}. *)
  end

  type 'v resolver
  (** A resolver key. *)

  val make : name:string -> 'v resolver
  (** Create a new resolver key. *)

  val name : 'v resolver -> string
  (** [name key] returns name of resolver [key]. *)

  val identifier : 'v resolver -> Identifier.t
  (** [identifier key] returns uniq identifier of resolver [key]. *)

  val empty : t
  (** An empty map of key resolvers. *)

  val add : 'v resolver -> resolve:(Domain_name.t -> 'v option IO.t) -> t -> t
  (** [add key impl t] returns a map containing the binding of [key] to [impl].
     If [key] was already bound in [t] to a value that is physically equal to
     [impl], [t] is returned unchanged. *)

  val rem : 'v resolver -> t -> t
  (** [remove key t] returns a map containing the same bindings as [t], expect
     for [key] which is unbound in the returned map. *)

  val get : 'v resolver -> t -> (Domain_name.t -> 'v option IO.t)
  (** [get key t] returns the current binding of [key] in [t]. *)

  val resolve : Domain_name.t -> 'v resolver -> t -> 'v option IO.t
  (** [resolve ~colored domain key t] tries to resolve [domain] with the binded
     resolver [key] with an already visited (colored) list of resolvers. *)
end
