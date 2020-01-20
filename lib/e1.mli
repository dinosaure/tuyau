module Refl : sig
  type ('a, 'b) t = Refl : ('a, 'a) t
end

type identifier = private int
val identifier_equal : identifier -> identifier -> bool
val identifier_compare : identifier -> identifier -> int

module Make (K : Sigs.FUNCTOR) (V : Sigs.FUNCTOR) : sig
  type 'a key

  module Key : sig
    type 'a info = 'a K.t

    val create : 'a info -> 'a key
    val info : 'a key -> 'a info
    val identifier : 'a key -> identifier

    type t

    val hide : 'a key -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
    val ( == ) : 'a key -> 'b key -> ('a, 'b) Refl.t option
  end

  type t

  val empty : t
  val is_empty : t -> bool
  val add : 'a key -> 'a V.t -> t -> t
  val mem : 'a key -> t -> bool
  val singleton : 'a key -> 'a V.t -> t
  val rem : 'a key -> t -> t
  val find : 'a key -> t -> 'a V.t option
  val len : t -> int

  type v = Value : 'a key * 'a V.t -> v

  val bindings : t -> v list
end
