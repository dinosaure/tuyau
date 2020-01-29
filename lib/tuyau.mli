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
  (** A [Flow] is an abstract value ['flow] and a module {!FLOW} which can use
     (parameterized with) ['flow]. From this value, you can {!FLOW.recv},
     {!FLOW.send} and {!FLOW.close} without details of underlying
     implementation.

      The idea behind this kind of value is to be able to implement logic of the
     client or the service without a knowledge of underlying protocol - which
     can be anything (eg. TCP, TCP + TLS, VCHAN, etc.). *)

  module type RESOLVER = Sigs.RESOLVER
    with type +'a s = 'a s

  type 'edn resolver = [ `host ] Domain_name.t -> ('edn option) s

  type 'edn key
  (** A representation of a {i resolver}. It hels you to bind a {i protocol} or
     a {i service} with an expected resolver (which must return an ['edn]). *)

  module Witness : sig
    type 'flow protocol
    (** A representation of a {i protocol}. It helps you to compose with others
       protocols or enforce the use of it when you want to initialize a
       connection (see {!flow}). *)

    type 't service
    (** A representation of a {i service}. It helps you to compose with others
       services. *)
  end

  module Map : sig
    type t

    val empty : t
    val is_empty : t -> bool

    val mem : 'a key -> t -> bool
    val rem : 'a key -> t -> t
    val len : t -> int
  end

  val key : ?priority:int -> string -> 'edn key
  val name_of_key : 'edn key -> string
  val priority : 'edn key -> int

  val register_service : key:'edn key -> service:('edn, 't, 'flow) service -> protocol:'flow Witness.protocol -> ('t * 'flow) Witness.service
  (** [register_service ~key ~service ~protocol] creates a representation of a
     {i service} (eg. {!Witness.service}). It binds [key] and [protocol] with
     the given implementation [service]. Resolver registered with [key] will be
     used to get the {i endpoint}. The given service is only able to deliver a
     {i flow} which can be {!abstract}ed with [protocol]. *)

  val register_protocol : key:'edn key -> protocol:('edn, 'flow) protocol -> 'flow Witness.protocol
  (** [register_protocol ~key ~protocol] creates a representation of a {i
     protocol} (eg. {!Witness.protocol}). It binds [key] with the given
     implementation [protocol]. Resolver registered with [key] will be used to
     get the {i endpoint}. The given protocol can create a ['flow] only from
     ['edn]. *)

  val register_resolver : key:'edn key -> 'edn resolver -> Map.t -> Map.t
  (** [register_resolver ~key resolver m] makes a new set of resolvers [m] with [resolver]. *)

  type error = [ `Msg of string | `Not_found | `Invalid_key | `Unresolved ]

  val pp_error : error Fmt.t

  val abstract : 'flow Witness.protocol -> 'flow -> flow
  (** [abstract protocol flow] returns the abstract type {!flow} from a concrete value ['flow] with
     the representation of the implementation [protocol]. *)

  val flow_of_endpoint : key:'edn key -> 'edn -> (flow, [> error ]) result s
  (** [flow_of_endpoint ~key edn] creates a new flow from the given endpoint
     ['edn]. You registered with {!register_protocol} a new protocol which can
     initialize a ['flow] from, for example, a [Unix.sockaddr] (with
     [Unix.connect]). A key [sockaddr : Unix.sockaddr key] must exist. In this
     context, the user is able to create a {!flow} from a given [Unix.sockaddr]:

     [|
      let sockaddr : Unix.sockaddr = Tuyau.key ~name:"sockaddr" ;;
      let mirage_io = Unix.gethostbyname "mirage.io" ;;

      flow_of_endpoint ~key:sockaddr mirage_io >>= fun (Tuyau.Flow (flow, (module Flow))) ->
      Flow.send flow "Hello World!"
     |] *)

  val flow_of_protocol : key:'edn key -> 'edn -> protocol:'flow Witness.protocol -> ('flow, [> error ]) result s
  (** [flow_of_protocol ~key edn ~protocol] creates a concrete ['flow] (the
     value {b is not} abstracted) from a given endpoint according a specific
     protocol. You registered with {!register_protocol} an UNIX TCP protocol
     which can initialize a [Unix.file_descr] with a [Unix.sockaddr]. In this
     context, the user is able to create the expected [Unix.file_descr]
     correctly initialized according the given endpoint [edn].

     [|
      let sockaddr : Unix.sockaddr = Tuyau.key ~name:"sockaddr" ;;
      let tcp : Unix.file_descr Witness.protocol = Tuyau.register_protocol ~key:sockaddr ~protocol:(module TCP) ;;
      let mirage_io = Unix.gethostbyname "mirage.io" ;;

      flow_of_protocol ~key:sockaddr mirage_io ~protocol:tcp >>= fun socket ->
      Unix.send socket "Hello World!"
     |] *)

  val flow : Map.t -> ?key:'edn key -> ?protocol:'flow Witness.protocol -> [ `host ] Domain_name.t -> (flow, [> error ]) result s
  (** [flow resolvers domain_name] tries to create a flow according resolvers.
     Each resolver will try to resolve [domain_name] and they give a set of
     endpoints. From them, we took the {i first} (or the specified) protocol
     which is able to create a {!flow}.

      If [key] is specified, only the associated resolver is used to try to
     resolve [domain_name].

      In others words:
     {ul
     {- [flow resolvers ~key domain_name] is [flow_of_endpoint ~key (Map.get key resolvers @@ domain_name)]}
     {- [flow resolvers ~key ~protocol domain_name] is [flow_of_protocol ~key ~protocol (Map.get key resolvers @@ domain_name)]}
     {- [flow resolvers ~protocol domain_name] is [flow_of_protocol ~key:(key_of_protocol) ~protocol (Map.get key resolvers @@ domain_name)]}
     {- [flow resolvers domain_name] is {!Conduit}}} *)

  val serve : key:'edn key -> 'edn -> service:('t * 'flow) Witness.service -> ('t * 'flow Witness.protocol, [> error ]) result s
  (** [serve ~key edn ~service] initialize a service from [edn] according
     [service]. It gives the concrete value of the service ['t] and the protocol
     used to communicate with the {i client}. *)

  val impl_of_service : key:'edn key -> ('t * 'flow) Witness.service -> ((module S with type endpoint = 'edn and type t = 't and type flow = 'flow), [> error ]) result
  val impl_of_protocol : key:'edn key -> 'flow Witness.protocol -> ((module F with type endpoint = 'edn and type flow = 'flow), [> error ]) result
  val impl_of_flow : 'flow Witness.protocol -> (module FLOW with type flow = 'flow)
end

module Make
    (Scheduler : Sigs.SCHEDULER)
    (Input : Sigs.SINGLETON)
    (Output : Sigs.SINGLETON)
  : S with type input = Input.t
       and type output = Output.t
       and type +'a s = 'a Scheduler.t
