module Make (IO : Sigs.IO) (B : Sigs.SINGLETON): sig
  module Service : module type of Service.Make (IO) (B)
  module Resolver : module type of Service.Resolver

  type 'e t
  type 'e action = 'e Service.action

  val make : name:string -> 'r Resolver.resolver -> ('r, 'e) Service.service -> 'e -> 'e t
  val bind : 'e t -> ('e -> 'e t) -> 'e t
  val map : 'e t -> ('e -> 'e) -> 'e t
  val action : 'e t -> ('e action -> 'a IO.t) -> 'a IO.t
  val resolve : Domain_name.t -> Resolver.t -> 'e t -> 'e t option IO.t
end
