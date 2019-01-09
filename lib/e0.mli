module Make (K : Sigs.FUNCTOR) : sig
  (* XXX(dinosaure): only on [>= 4.06.0] *)
  type t = private ..

  module type Extension = sig
    type x
    type t += T of x

    val instance : x K.t
  end

  type 'a extension = (module Extension with type x = 'a)
  type instance = V : 'a * 'a K.t -> instance

  module Injection (X : sig
    type t

    val instance : t K.t
  end) : Extension with type x = X.t

  val inj : 'a K.t -> 'a extension
  val prj : t -> instance
  val extract : t -> 'a extension -> 'a option
end
