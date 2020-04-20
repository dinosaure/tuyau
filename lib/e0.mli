module Make (Key : Sigs.FUNCTOR) : sig
  (* XXX(dinosaure): only on [>= 4.06.0] *)
  type t = private ..

  module type S = sig
    type x
    type t += T of x

    val witness : x Key.t
  end

  type 'a s = (module S with type x = 'a)
  type v = Value : 'a * 'a Key.t -> v
  type k = Key : 'a Key.t * ('a -> t) -> k

  val inj : 'a Key.t -> 'a s
  val prj : t -> v
  val extract : t -> 'a s -> 'a option
  val bindings : unit -> k list
end
