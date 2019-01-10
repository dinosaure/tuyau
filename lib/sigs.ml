module type FUNCTOR = sig type 'a t end
module type SINGLETON = sig type t end

module type IO = sig
  type +'a t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val map : ('a -> 'b) -> 'a t -> 'b t
  val return : 'a -> 'a t

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
end

type 'a or_eoi = [ `Data of 'a | `Eoi ]

module type FLOW = sig
  type +'a io
  type flow
  type error
  type write_error
  type buffer

  val pp_error : error Fmt.t
  val pp_write_error : write_error Fmt.t
  val read : flow -> (buffer or_eoi, error) result io
  val write : flow -> buffer -> (int, write_error) result io
  val close : flow -> (unit, error) result io
end

module type SERVICE = sig
  type description
  type endpoint

  type buffer
  type +'a io

  include FLOW with type +'a io := 'a io
                and type buffer := buffer

  val init : description -> endpoint -> flow -> (flow, error) result io
end
