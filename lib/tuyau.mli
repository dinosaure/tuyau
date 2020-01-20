module Sigs = Sigs

module type S = sig
  type input
  type output

  type +'a s

  module type S = Sigs.S
    with type +'a s = 'a s

  module type F = Sigs.F
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type ('edn, 't, 'flow) service =
    (module S with type endpoint = 'edn
               and type t = 't
               and type flow = 'flow)
  type ('edn, 'flow) protocol =
    (module F with type endpoint = 'edn
               and type flow = 'flow)

  module type FLOW = Sigs.FLOW
    with type input = input
     and type output = output
     and type +'a s = 'a s

  type flow = Flow : 'flow * (module FLOW with type flow = 'flow) -> flow

  module type RESOLVER = Sigs.RESOLVER
    with type +'a s = 'a s

  type 'edn resolver = [ `host ] Domain_name.t -> ('edn option) s
  type 'edn key

  module Witness : sig
    type 'flow protocol
    (** A representation of a {i protocol}. *)

    type 't service
    (** A representation of a {i service}. *)
  end

  module Map : sig
    type t

    val empty : t
    val is_empty : t -> bool

    val mem : 'a key -> t -> bool
    val rem : 'a key -> t -> t
    val len : t -> int
  end

  val key : name:string -> 'edn key

  val register_service : key:'edn key -> service:('edn, 't, 'flow) service -> protocol:'flow Witness.protocol -> ('t * 'flow) Witness.service
  (** [register_service ~key ~service ~protocol] creates a representation of a
     {i service}. It binds [key] and [protocol] with the given implementation
     [service]. Resolver registered with [key] will be used to get the {i
     endpoint}. The given service is only able to deliver a {i flow} which can
     be {!lift} with [protocol]. *)

  val register_protocol : key:'edn key -> protocol:('edn, 'flow) protocol -> 'flow Witness.protocol
  val register_resolver : key:'edn key -> 'edn resolver -> Map.t -> Map.t

  type error = [ `Msg of string | `Not_found | `Invalid_key | `Unresolved ]

  val pp_error : error Fmt.t

  type p

  val lift : 'flow -> 'flow Witness.protocol -> (p, [> error ]) result s
  val unlift : p -> flow

  val flow_of_endpoint : key:'edn key -> 'edn -> (p, [> error ]) result s
  val flow_of_protocol : key:'edn key -> 'edn -> protocol:'flow Witness.protocol -> ('flow, [> error ]) result s
  val flow : Map.t -> ?key:'edn key -> ?protocol:'flow Witness.protocol -> [ `host ] Domain_name.t -> (p, [> error ]) result s

  val service : key:'edn key -> 'edn -> service:('t * 'flow) Witness.service -> ('t * 'flow Witness.protocol, [> error ]) result s

  val server : key:'edn key -> ('t * 'flow) Witness.service -> ((module S with type endpoint = 'edn and type t = 't and type flow = 'flow), [> error ]) result
  val protocol : key:'edn key -> 'flow Witness.protocol -> ((module F with type endpoint = 'edn and type flow = 'flow), [> error ]) result
end

module Make
    (Scheduler : Sigs.SCHEDULER)
    (Input : Sigs.SINGLETON)
    (Output : Sigs.SINGLETON)
  : S with type input = Input.t
       and type output = Output.t
       and type +'a s = 'a Scheduler.t
