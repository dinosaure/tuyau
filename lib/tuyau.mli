module Sigs = Sigs

(** [Tuyau] is a little library which wants to give to the developer the
   easiest way to compose protocols and only one way to make a {i Flow}. Several
   words are used in this sentence and we need a clear definition of them to
   fully understand the purpose of [Tuyau].

    {3 A Protocol.}

    A communication protocol is a system of rules that allows entities to
   transmit information. In the case of [Tuyau], this kind of information must not
   be arbitrary. The protocol should only solve communication problems such as
   {i routing}.

    When we talk about a protocol, it's only about a standard which is able to
   transmit a {i payload}. Interpretation of the {i payload} is not done by the
   {i protocol} but by the user of this library.

    For example, the Transmission Control Protocol (TCP) {b is} a protocol
   according to [Tuyau] because it is able to transmit {i payload} without
   interpreting it. A counter example is the Simple Mail Transfer Protocol
   (SMTP) which gives an interpretation of the {i payload} (such as [EHLO] which
   is different to [QUIT]).

    This difference is important to unlock the ability to compose {i protocols}.
   An other protocol according to [Tuyau] is Transport Layer Security (TLS) -
   which wants to solve privacy and data integrity. [Tuyau] is able to compose
   protocols together like [TCP ∘ TLS] to make a new protocol. From this
   composition, the user is able to implement Secure Simple Mail Transfer
   Protocol (SSMTP) or HyperText Transfer Protocol Secure (HTTPS) - both use TCP
   and TLS.

    {3 A Flow.}

    To be able to do this composition, the protocol must respect an interface:
   the [FLOW] interface. It defines an abstract type [t] and functions like
   [recv] or [send]. These functions give to us the {i payload}. Rules to solve
   communication problems are already processed internally.

    In other terms, from a given [FLOW], the user should not handle {i routing},
   privacy or data integrity (or some others problems). The user should only be
   able to process the {i payload}.

    Finally, representation of a TCP protocol is a [FLOW]. VCHAN protocol or
   User Datagram Protocol (UDP) can be represented by a [FLOW]. However, TLS is
   not a flow but a layer on top of another protocol. Composition with it should
   look like:

    {[ val with_tls : (module FLOW) -> (module FLOW) ]}

    From a given [FLOW], we {i wrap} it with TLS and return a new [FLOW]. Such a
   composition exists also for WireGuard or Noise layers. [Tuyau] wants to solve
   this composition by a strict OCaml interface of the [FLOW].

    {3 Resolution.}

    [Tuyau] wants to solve one last problem, resolution of an {i endpoint}. The
   goal is to make a [FLOW] from an {i endpoint} given by the developer.

    Definition of an endpoint can not fully exist where it depends on the
   returned [FLOW]. For example, if we give to you a TCP flow, {i endpoint}
   should be an IP and a {i port} where the given [FLOW] is {b already}
   connected.

    However, we agree that the most general (by convention) description of the
   {i endpoint} is the domain-name. By knowing this, we let the developer to
   construct an {i endpoint} from a [[ `host ] Domain_name.t].

    At the end, [Tuyau] should be able to construct an {i endpoint} from a [[
   `host ] Domain_name.t]. Then, it tries to find a [SERVICE] according to the
   given {i endpoint} and initializes a [FLOW].

    The most abstract definition provided by [Tuyau] is:

    {[ val flow : resolvers -> [ `host ] Domain_name.t -> flow ]}

    Where [resolvers] is a set of {i heterogeneous} constructors of {i
   endpoints} given by the developer. The returned value [flow] is an
   abstraction of an {b already} initialized communication protocol. From it,
   the developer can {i extract} [send] and [recv] functions (as described into
   {!A Protocol}).

    {3 Conclusion.}

    [Tuyau] is a {i framework} which wants to give a few definitions to {b
   restrict} developers of protocols to an interface [FLOW] and, by this way,
   provide them with a set of tools to compose with others protocols and give
   only one way to resolve an {i endpoint} (whatever its definition).

    [Tuyau] does not make magic and all described processes previously are
   explicit - composition, resolution, extraction. This last aspect wants to
   solve a well-known problem of [Conduit] where nobody can fully understand this
   framework.

    You can start to read the rest of the documentation. *)

module type S = sig
  type input
  (** The type of the {i input}. A flow is able to {i send} a {i payload}. The
     type of the {i payload} is [input]. *)

  type output
  (** The type of the {i output}. A flow is able to {i receive} a {i payload}.
     The type of the {i payload} is [output]. *)

  (** {3 Input & Output.}

      Type of input can differ to type of output to have the ability to define
     capabilities on them such as the {i read} capability or the {i write}
     capability. A {i caml} example looks like:

      {[
        type input = bytes
        type output = string
      ]} *)

  type +'a s
  (** The type of {i scheduler}. [Tuyau] is able to call some {i syscall} which
     can be wrap in a {i monad} such as LWT or ASYNC. The core [Tuyau] library
     is abstracted over that. *)

  (** {3 Scheduling.}

      [Tuyau] does not do the choice about LWT or ASYNC (or UNIX). However, it
     should be able to call any {i syscall} (like [Unix.connect]) which can be
     {i wrap} into a {i monad}. By this way, the core library is not specialized
     to a specific {i backend}.

      However, this specialization is done as soon as we can. So, [Tuyau_unix],
     [Tuyau_mirage] or [Tuyau_caml] are different and can not be used together
     into a same place. *)

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

  (** A [flow] is an abstract value ['flow] and a module {!FLOW} (parameterized
     with ['flow]). The value ['flow] is already connected to an {i endpoint}
     and the module {!FLOW} can not give to the user a way to re-create
     (re-connect) the flow. However, it gives a way to receive and send {i
     payload}:

      {[
        type input = bytes
        type output = string

        type +'a s = 'a

        let process (Flow (flow, (module Flow))) =
          let buf = Bytes.create 0x1000 in
          match Flow.recv flow buf 0 0x1000 with
          | Ok (`Data len) ->
            let str = Bytes.sub_string buf 0 len in
            ignore (Flow.send flow str 0 len)
          | _ -> failwith "Flow.recv"
      ]}
  *)
  type flow = Flow : 'flow * (module FLOW with type flow = 'flow) -> flow

  module type RESOLVER = Sigs.RESOLVER
    with type +'a s = 'a s

  type 'edn resolver = [ `host ] Domain_name.t -> ('edn option) s
  (** A [resolver] is an abstract function which resolves a given [[ `host ]
     Domain_name.t] to an {i endpoint}. At least, it can be implemented as a DNS
     resolver such as:

      {[
        type +'a s = 'a

        let http_resolver : Unix.sockaddr resolver = fun domain_name ->
          match Unix.gethostbyname (Domain_name.to_string domain_name) with
          | { Unix.h_addr_list; _ } ->
            if Array.length h_addr_list > 0
            then Some (Unix.ADDR_INET (h_addr_list.(0), 80))
            else None
          | _ -> None
      ]}

      Definition of {i endpoint} is free as long as a protocol can
     initialize/connect a {!FLOW.flow} from it. In our example, a [Unix] TCP
     service should exist with [Unix.connect]. *)

  type 'edn key
  (** To be able to {i plug} a {!resolver} to a {!service} or a {!protocol}, a
     value ['edn key] exists. It represents, at the resolution step, {!protocol}
     into an user-defined {!Map.t}.

      Any construction of a {!service} or a {!protocol} give to us a ['edn key]
     like a [Unix.sockaddr key] for example. The user has the ability to
     construct then a restrained way to resolve a [[ `host ] Domain_name.t]: a
     set of {i heterogeneous} constructors of {i endpoint}.

      Each constructor of {i endpoint} is bound with a ['edn key]. If one of
     them is able to resolve the given domain-name, by the ['edn key], [Tuyau]
     is able to invoke the right {!protocol} to process the initialization.

      {[
        val tcp_protocol : (Unix.sockaddr, Unix.file_descr) protocol
        val tcp_endpoint : Unix.sockaddr key

        val http_resolver : Unix.sockaddr resolver (* on [*:80] *)
        val debug_http_resolver : Unix.sockaddr resolver (* on [*:8080] *)

        let map =
          Map.empty
          |> register_resolver ~key:tcp_endpoint ~priority:10 http_resolver
          |> register_resolver ~key:tcp_endpoint ~priority:20 debug_http_resolver
      ]} *)

  module Witness : sig
    type 'flow protocol
    type 't service
  end

  module Map : sig
    type t

    val empty : t
    val is_empty : t -> bool

    val mem : 'a key -> t -> bool
    val rem : 'a key -> t -> t
    val len : t -> int
  end

  val key : string -> 'edn key
  (** [key name] creates a new key. The returned value can be bound to a
     {!service} with {!register_service} or a {!protocol} with
     {!register_protocol}.

      The goal of the returned value is to plug a {!resolver} without any
     knowledge of the the {!protocol}.

      {[
        type input = bytes
        type output = string

        type +'a s = 'a

        module Tuyau_tcp : sig
          val key : Unix.sockaddr key
        end = struct
          let key : Unix.sockaddr key = key "sockaddr"
          let protocol = register_protocol ~key ~protocol:(module TCP)
        end

        let resolvers =
          Map.empty
          |> register_resolver ~key:Tuyau_tcp.key http_resolver
          |> register_resolver ~key:Tuyau_tcp_tls.key https_resolver

        let mirage_io =
          Domain_name.(host_exn <.> of_string_exn) "mirage.io"

        let () =
          match flow resolves mirage_io with
          | Ok (flow, (module Flow)) ->
            ignore (Flow.send flow "Hello World!")
          | Error err -> failwithf "%a" pp_error err
      ]} *)

  val name_of_key : 'edn key -> string

  (** {3 Registration.} *)

  val register_service : key:'edn key -> service:('edn, 't, 'flow) service -> protocol:'flow Witness.protocol -> ('t * 'flow) Witness.service
  (** [register_service ~key ~service ~protocol] registers implementation of a
     {i service} which is able to make a {i flow} (an established transmission
     between the service and an entity) according to the given definition
     [protocol]. It binds [service] with [key] to be able to correctly
     initialize the given service.

      A {!service} is not use with the resolution process because we assert that
     the initialization of any service should be fully know. [key] unlocks only
     the ability to let the user to define his type of {i endpoint}/{i
     configuration} - at this stage, and only about {!service}, goal of [key]
     differs from {!register_protocol}.

      {[
        module TCP_service : S with type configuration = Unix.sockaddr
                                and type t = Unix.file_descr
                                and type flow = TCP.t (* = Unix.file_descr *)

        let key : Unix.sockaddr = key "sockaddr"
        let service : (Unix.file_descr * TCP.t) Witness.service =
          register_service ~key ~service:(module TCP_service) ~protocol:TCP.protocol
      ]} *)

  val register_protocol : key:'edn key -> protocol:('edn, 'flow) protocol -> 'flow Witness.protocol
  (** [register_protocol ~key ~protocol] registers implementation of a {i
     protocol} and binds it with [key] - any resolver bound into a {!Map.t} with
     this [key] will call (at least) [connect] given by [protocol].

      [protocol] is an OCaml module which respects the interface {!F} (a
     specialization of {!FLOW} according {!input}, {!output} and {!s}).

      The returned value is a {i light} representation of the given [protocol]
     which can be use by the user for some others processes like the
     composition.

      {[
        module TCP : F with type endpoint = Unix.sockaddr
                        and type t = Unix.file_descr

        let key : Unix.sockaddr key = key "sockaddr"
        let protocol : Unix.file_descr Witness.protocol =
          register_protocol ~key ~protocol:(module TCP)
      ]} *)

  val register_resolver : key:'edn key -> ?priority:int -> 'edn resolver -> Map.t -> Map.t
  (** [register_resolver ~key ?priority resolver m] adds a new [resolver] into
     [m]. [resolver] is bound to [key]. From a set of [key] which represent the
     way to initialize a {!protocol}, we can bind a [resolver] into [m].

      When the [resolver] is able to resolve the given domain-name, it will try
     to initialize the transmission over the protocol bound to the shared [key].
     We try resolvers to a specific order (lower to higher).

      {[
        val resolver_on_my_private_network : Unix.sockaddr resolver
        val resolver_on_internet : Unix.sockaddr resolver

        let m =
          Map.empty
          |> register_resolver ~key:tcp_endpoint ~priority:10 resolver_on_my_private_network
          |> register_resolver ~key:tcp_endpoint ~priority:20 resolver_on_internet
      ]} *)

  type error = [ `Msg of string | `Not_found | `Invalid_key | `Unresolved ]

  val pp_error : error Fmt.t

  val abstract : 'flow Witness.protocol -> 'flow -> flow
  (** [abstract protocol flow] constructs an abstracted value {!flow} from a
     representation of the implementation of the protocol ([protocol]) and an
     already initialized [flow]. *)

  val flow_of_endpoint : key:'edn key -> 'edn -> (flow, [> error ]) result s
  (** [flow_of_endpoint ~key edn] creates a new abstracted flow from the given
     endpoint ['edn]. Protocol used to initialize the transmission is (already)
     registered with {!register_protocol} and [key].

      User can register more than one protocol with the given [key]. In this
     case, all of these protocols are extracted and they try to initialize the
     transmission. The first which initializes the transmission is taken to
     return the {!flow}. The order of protocols is undefined.

      {[
        let sockaddr : Unix.sockaddr = Tuyau.key "sockaddr"
        let tcp : Unix.file_descr Witness.protocol
        let udp : Unix.file_descr Witness.protocol

        let mirage_io : Unix.sockaddr = match Unix.gethostbyname "mirage.io" with
          | { Unix.h_addr_list; _ } ->
            if Array.length h_addr_list > 0
            then Unix.ADDR_INET (h_addr_list.(0), 4242)
            else failwith "Impossible to resolver mirage.io"

        let () = match flow_of_endpoint ~key:sockaddr mirage_io with
          | Ok (flow, (module Flow)) ->
            ignore (Flow.send flow "Hello World!")
          | Error err -> failwithf "%a" pp_error err
      ]} *)

  val flow_of_protocol : key:'edn key -> 'edn -> protocol:'flow Witness.protocol -> ('flow, [> error ]) result s
  (** [flow_of_protocol ~key edn ~protocol] creates a new concrete ['flow] from
     the given endpoint ['edn]. Protocol used to initialize the transmission is
     (and only is) [protocol].

      {[
        let sockaddr : Unix.sockaddr = Tuyau.key "sockaddr"
        let tcp : Unix.file_descr Witness.protocol

        let mirage_io : Unix.sockaddr = match Unix.gethostbyname "mirage.io" with
          | { Unix.h_addr_list; _ } ->
            if Array.length h_addr_list > 0
            then Unix.ADDR_INET (h_addr_list.(0), 4242)
            else failwith "Impossible to resolver mirage.io"

        let () = match flow_of_protocol ~key:sockaddr ~protocol:tcp mirage_io with
          | Ok fd ->
            ignore (Unix.write fd "Hello World!" 0 12)
          | Error err -> failwithf "%a" pp_error err
      ]} *)

  (** {3 [Tuyau] as a client.} *)

  val flow : Map.t -> ?key:'edn key -> ?protocol:'flow Witness.protocol -> [ `host ] Domain_name.t -> (flow, [> error ]) result s
  (** [flow resolvers domain_name] tries to create a new abstracted according to
     [resolvers]. Each resolver tries to resolve the given domain-name (they are
     ordered by the given priority). Then, from a {i heterogeneous} set of {i
     endpoints}, we try to initialize/establish a transmission. The first which
     initializes the connection is taken to return the {!flow}.

      User can enforce to use a specific [key] and, by this way, a specific
     resolver instead to call all of them (available into [resolvers]).

      User can enforce to use a specific [protocol], and by this way, enforce to
     use a specific [key] (which is bound by [protocol]).

      {[
        let mirage_io = Domain_name.(host_exn <.> of_string_exn) "mirage.io"

        val resolver_on_my_private_network : Unix.sockaddr resolver
        val resolver_on_internet : Unix.sockaddr resolver
        val resolver_with_tls : Tls.Config.client -> Unix.sockaddr resolver

        let resolvers =
          Map.empty
          |> register_resolver ~key:tls_endpoint ~priority:0 (resolver_with_tls tls_config)
          |> register_resolver ~key:tcp_endpoint ~priority:10 resolver_on_my_private_network
          |> register_resolver ~key:tcp_endpoint ~priority:20 resolver_on_internet

        let () = match flow resolvers mirage_io with
          | Ok (flow, (module Flow)) ->
            ignore (Flow.send flow "Hello World!")
          | Error err -> failwithf "%a" pp_error err
      ]} *)

  (** {3 [Tuyau] as a server.} *)

  val serve : key:'edn key -> 'edn -> service:('t * 'flow) Witness.service -> ('t * 'flow Witness.protocol, [> error ]) result s
  (** [serve ~key edn ~service] creates a new {i master} server with which {i
     protocol} it can deliver according a configuration ['edn]. [serve] is more
     restrictive than {!flow} when we assert that the initialization of a
     service should be fully know.

      The initialization of the service returns a concrete type ['t] which
     represents the service. It returns which protocol is used to transmit
     information with entities.

      {[
        val sockaddr : Unix.sockaddr key
        val tcp_service : (Unix.file_descr * TCP.t) Witness.service

        let () =
          impl_of_service ~key:sockaddr tcp_service |> get_ok |> fun (module Server) ->
          match serve ~key:sockaddr Unix.(ADDR_INET (inet_addr_any, 8080)) tcp_service with
          | Ok (master, protocol) ->
            let module Flow = impl_of_flow protocol in
            let rec go () = match Server.accept t with
              | Ok flow ->
                ignore (Flow.send flow "Hello World") ;
                Flow.close flow ;
                go ()
              | Error err -> failwithf "%a" Server.pp_error err in
            go ()
      ]} *)

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
