module type SERVICE = sig
  type description
  type endpoint
  type from

  type buffer
  type +'a io

  val init : description -> from -> endpoint -> endpoint io
  val read : endpoint -> buffer -> int io
  val write : endpoint -> buffer -> int io
end

module Make (IO : Sigs.IO) (B : Sigs.SINGLETON) : sig
  module Resolver : module type of Resolver.Make(IO)

  type kind = UDP | TCP

  type desc =
    { name : string
    ; port : int
    ; kind : kind }

  type ('r, 'e) init = desc -> 'r -> 'e -> 'e IO.t
  type 'e read = 'e -> B.t -> int IO.t
  type 'e write = 'e -> B.t -> int IO.t

  type ('r, 'e) service =
    { desc : desc
    ; init : ('r, 'e) init
    ; rd : 'e read
    ; wr : 'e write }

  type 'e action =
    { rd : 'e -> B.t -> int IO.t
    ; wr : 'e -> B.t -> int IO.t }

  module type SERVICE = SERVICE with type description = desc and type 'a io = 'a IO.t and type buffer = B.t

  val of_module : name:string -> port:int -> kind:kind ->
    (module SERVICE with type endpoint = 'e
                     and type from = 'r)
    -> ('r, 'e) service

  type 'e scheme

  type endpoint

  val add : 'r Resolver.resolver -> ('r, 'e) service -> 'e scheme
  val endpoint : 'e scheme -> 'e -> endpoint
  val resolve : Domain_name.t -> Resolver.t -> 'e scheme -> endpoint -> endpoint option IO.t

  val instance : 'e scheme -> ('e action -> 'a) -> 'a
  val bind : 'e scheme -> endpoint -> ('e -> endpoint) -> endpoint option
  val map : 'a scheme -> 'b scheme -> endpoint -> ('a -> 'b) -> endpoint option
  val return : 'e scheme -> 'e -> endpoint
end
