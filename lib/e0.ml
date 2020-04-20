(* (c) Frédéric Bour *)

module Make (Key : Sigs.FUNCTOR) = struct
  type t = ..

  module type S = sig
    type x
    type t += T of x

    val witness : x Key.t
  end

  type 'a s = (module S with type x = 'a)
  type v = Value : 'a * 'a Key.t -> v
  type k = Key : 'a Key.t * ('a -> t) -> k

  let handlers = Hashtbl.create 16
  let witnesses = Hashtbl.create 16

  module Injection (X : sig
    type t

    val witness : t Key.t
  end) : S with type x = X.t = struct
    type x = X.t
    type t += T of x

    let witness = X.witness

    let () =
      let[@warning "-3"] uid = Stdlib.Obj.extension_id [%extension_constructor T] in
      Hashtbl.add handlers uid
        (function T x -> Value (x, witness) | _ -> raise Not_found) ;
      Hashtbl.add witnesses uid (Key (witness, (fun x -> T x)))
  end

  let inj (type a) (k : a Key.t) : a s =
    ( module Injection (struct
      type t = a

      let witness = k
    end) )

  let prj (t : t) =
    let rec go = function
      | [] -> assert false (* totality *)
      | f :: r -> ( try f t with Not_found -> go r ) in
    go
      (Hashtbl.find_all handlers
         Stdlib.Obj.((extension_id (extension_constructor t))
                      [@warning "-3"]))

  let extract (t : t) (type a) ((module S) : a s) : a option = match t with
    | S.T x -> Some x
    | _ -> None

  let bindings : unit -> k list =
    fun () -> Hashtbl.fold (fun _ v a -> v :: a) witnesses []
end
