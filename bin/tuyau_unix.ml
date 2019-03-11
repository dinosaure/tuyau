let () = Printexc.record_backtrace true

(* No LWT, no ASYNC ... *)

module Noop : Tuyau.Sigs.IO with type +'a t = 'a = struct
  type +'a t = 'a

  let return x = x
  let bind x f = f x
  let map f x = f x

  let ( >>= ) = bind
  let ( >|= ) x f = map f x
end

module High = Tuyau.High.Make (Noop) (Bytes)
open High

(* I define my [socket] type which has an information for the [read] operation,
   how many bytes we will allocates to read the flow. Of course, we have the
   [Unix.file_descr] too. *)

type socket =
  { socket : Unix.file_descr
  ; chunk : int }

(* Implementation of the server on my [socket]. Server expects a
   [Unix.inet_addr] to be initialized. It can be anythit, [unit],
   [Domain_name.t], etc. *)

module Server
  : Service.SERVICE with type endpoint = Unix.inet_addr
                     and type flow = socket
= struct
  type description = Service.desc
  type endpoint = Unix.inet_addr
  type flow = socket

  type buffer = Bytes.t
  type +'a io = 'a Noop.t

  type error = [ `Never ]
  type write_error = [ `Never ]

  let pp_error _ `Never = assert false
  let pp_write_error _ `Never = assert false

  (* A point about [desc] (NOTE: I wrote this comment at the end, it's better to
     miss it and read it at the end). [desc] is the description of the service.
     I mean, this module IS NOT a service, it's just an implementation about the
     flow and how to initialize the flow.

     Then, if this implementation was used as an echo service, we can know it by
     [desc]. But keep in your mind, this __is not__ a service. The [desc] tells
     you which port the service binded to this implementation you should use and
     some stuffs - like it's over TCP or UDP and the name of the service. *)

  let make desc inet_addr =
    let socket = { socket= Unix.socket Unix.PF_INET SOCK_STREAM 0
                 ; chunk = 0x100 } in
    Fmt.pr "Server start to listen.\n%!" ;
    Unix.bind socket.socket (Unix.ADDR_INET (inet_addr, desc.Service.port)) ;
    Unix.listen socket.socket 40 (* get it from [inet_addr] or [socket]? *) ; Ok socket

  let read { socket; chunk; } =
    let bytes = Bytes.create chunk in
    let n = Unix.read socket bytes 0 (Bytes.length bytes) in
    if n = 0 then Ok `Eoi else Ok (`Data (Bytes.sub bytes 0 n))

  let write { socket; _ } bytes = let n = Unix.write socket bytes 0 (Bytes.length bytes) in Ok n
  let close { socket; _ } = let () = Unix.close socket in Ok ()
end

(* Implementation of the client on [Unix.file_descr] directly. To see power of
   [tuyau]. Client expects a [Unix.sockaddr] instead a [Unix.inet_addr]. This
   diff is to show that [endpoint] can be anything and [flow] can be anything
   too. User of [tuyau] can define what he wants. *)

module Client
  : Service.SERVICE with type endpoint = Unix.sockaddr
                     and type flow = Unix.file_descr
= struct
  type description = Service.desc
  type endpoint = Unix.sockaddr
  type flow = Unix.file_descr

  type buffer = Bytes.t
  type +'a io = 'a Noop.t

  type error = [ `Never ]
  type write_error = [ `Never ]

  let pp_error _ `Never = assert false
  let pp_write_error _ `Never = assert false

  let pp_inet_addr ppf _ = Fmt.string ppf "#inet-addr"

  let pp_sockaddr ppf = function
    | Unix.ADDR_UNIX service -> Fmt.pf ppf "<%s>" service
    | Unix.ADDR_INET (inet_addr, port) -> Fmt.pf ppf "%a:%d" pp_inet_addr inet_addr port

  let make desc sockaddr =
    Fmt.pr "Client start to connect to %a.\n%!" pp_sockaddr sockaddr ;
    let fd = Unix.socket Unix.PF_INET SOCK_STREAM 0 in
    let with_port = function
      | Unix.ADDR_UNIX _ as v -> v
      | Unix.ADDR_INET (inet_addr, _) -> Unix.ADDR_INET (inet_addr, desc.Service.port) in
    Unix.connect fd (with_port sockaddr) ; Ok fd

  let read fd =
    let bytes = Bytes.create 0x100 in
    let n = Unix.read fd bytes 0 (Bytes.length bytes) in
    if n = 0 then Ok `Eoi else Ok (`Data (Bytes.sub bytes 0 n))

  let write fd bytes = let n = Unix.write fd bytes 0 (Bytes.length bytes) in Ok n
  let close fd = let () = Unix.close fd in Ok ()
end

(* We wrap services (server and client) with a description (in this case, a
   shared description). At this stage, terms are really important. [Client] and
   [Server] are not a service. They are just an implementation.

   The point is: two service can have the same implementation. *)

let echo_service = Service.of_module ~name:"echo" ~port:8090 ~kind:Service.TCP (module Server)
let client_service = Service.of_module ~name:"client echo" ~port:8090 ~kind:Service.TCP (module Client)

(* Boilerpart, implementation of the f*cking unsafe [read_line]. *)

let blit_bigstring_to_bytes src src_off dst dst_off len =
  for i = 0 to len - 1
  do Bytes.set dst (dst_off + i) (Bigarray.Array1.get src (src_off + i)) done

let blit_bytes_to_bigstring src src_off dst dst_off len =
  for i = 0 to len - 1
  do Bigarray.Array1.set dst (dst_off + i) (Bytes.get src (src_off + i)) done

exception Break

let has_line buf =
  let where = ref 0 in
  try Ke.Rke.iter (fun chr -> if chr = '\n' then raise Break else incr where) buf ; None
  with Break -> Some !where

let read_line
  : type f error. (module Service.FLOW with type flow = f and type error = error)
    -> f
    -> [ `Eoi of string | `Line of string | `Error of error ]
  = fun (module Flow) ->
  let buf = Ke.Rke.create ~capacity:0x100 Bigarray.Char in
  fun flow ->
    let rec loop flow = match Flow.read flow with
      | Error err -> `Error err
      | Ok `Eoi ->
        let len = Ke.Rke.length buf in
        let res = Bytes.create len in
        Ke.Rke.N.keep_exn buf ~blit:blit_bigstring_to_bytes ~length:Bytes.length ~len res ;
        Ke.Rke.N.shift_exn buf len ;
        `Eoi (Bytes.unsafe_to_string res)
      | Ok (`Data data) ->
        Ke.Rke.N.push buf ~blit:blit_bytes_to_bigstring ~length:Bytes.length data ;
        match has_line buf with
        | Some len ->
          let res = Bytes.create len in
          Ke.Rke.N.keep_exn buf ~blit:blit_bigstring_to_bytes ~length:Bytes.length ~off:0 ~len res ;
          Ke.Rke.N.shift_exn buf (len + 1) ;
          `Line (Bytes.unsafe_to_string res)
        | None -> loop flow in
    loop flow

(* Logic of the server part AS an echo service. At this stage, we really talk about the service.
   Like HTTP service, ECHO service (in this case), etc. *)

let handle_message
  : type f. f flow -> f -> unit
  = fun (module Flow) flow ->
  let read_line = read_line (module Flow) in

  let rec go () = match read_line flow with
    | `Eoi trailer -> Fmt.pr "Client gave up with %s.\n%!" trailer
    | `Line "ping" ->
      Fmt.pr "Server> receive ping.\n%!" ;
      let _ = Flow.write flow (Bytes.of_string "pong\n") in go ()
    | `Line response ->
      let _ = Flow.write flow (Bytes.of_string response) in
      Fmt.pr "Server> Unsupported response: %s.\n%!" response ;
      go ()
    | `Error err -> Fmt.epr "Server> Retrieve an error: %a.\n%!" Flow.pp_error err in
  go ()

let handle_connection ~chunk impl conn =
  let fd, _ = conn in
  (* this line proves that we know everything between [impl] and [socket]. We
     are able to use it, construct it and so on. *)
  handle_message impl { chunk; socket= fd }

(* The most interesting part, I think. In this code, we receive an abstract
   value [flow]. However, by a black magic, we can extract from it [socket] and
   associated implementation. In this case, we extract [socket] and [Server]
   module (as [impl]). Because we know well the definition of both, we can use
   then as they are.

   So, because we know it's a [socket], we can destruct it, take the master
   [socket] and use it with [Unix.accept]. [Service.init] was already binded
   [master] on an endpoint. *)

let server t =
  High.extract t (* extract action *) @@ fun ({ socket; chunk; } as master) impl ->
  let rec loop : unit -> unit = fun () ->
    let conn = Unix.accept socket in
    let () = handle_connection ~chunk impl conn in
    loop () in
  loop () ; master

(* Logic of the client part AS an echo service. *)

(* Like the server part, the client part use the same model. We extract it and
   we can use it directly as a [Unix.file_descr] (because we define
   [Client.flow] as it). *)

let client t =
  let loop : type f. f ->  f flow -> f = fun fd (module Flow) ->
    let read_line = read_line (module Flow) in
    let rec loop : unit -> unit = fun () ->
      let _ = Flow.write fd (Bytes.of_string "ping\n") in
      match read_line fd with
      | `Eoi trailer -> Fmt.pr "Server gave up with: %s.\n%!" trailer
      | `Line "pong" -> Fmt.pr "Client> receive pong.\n%!" ; loop ()
      | `Line response ->
        Fmt.pr "Client> Unsupported response: %s.\n%!" response ;
        loop ()
      | `Error err -> Fmt.epr "Client> Retrieve an error: %a.\n%!" Flow.pp_error err in
    loop () ; fd in
  High.extract t loop

(* Resolvers

   In this part, we will implement two kind of resolvers. One which use
   [Unix.gethostbyname] and the other which uses [Unix.getaddrinfo]. The
   interesting part is both return different values - the first returns a
   [Unix.inet_addr], the second returns a [Unix.sockaddr].

   If you remember correctly, the server implementation expect an
   [Unix.inet_addr] and the client expects a [Unix.sockaddr].

   A resolver must take a [Domain_name.t] - don't think to anything else a open
   Hell's door. *)

let gethostbyname domain =
  match Unix.gethostbyname (Domain_name.to_string domain) with
  | { Unix.h_addr_list= arr; _ } ->
    Fmt.pr "Resolve gethostbyname: %a (found %d solution(s)).\n%!" Domain_name.pp domain (Array.length arr) ;
    if Array.length arr > 0 then Some arr.(0) else None

let getaddrinfo domain =
  match Unix.getaddrinfo (Domain_name.to_string domain) "" [] with
  | { Unix.ai_addr; _ } :: _ as lst ->
    Fmt.pr "Resolve getaddrinfo: %a (found %d solution(s)).\n%!" Domain_name.pp domain (List.length lst) ;
    Some ai_addr
  | _ -> None

(* Then, we need to create (side-effect here!) keys which are not associated to
   an implementation yet! They are just a description of what they give to the
   [init] function. *)

let inet_addr_resolver : Unix.inet_addr Resolver.resolver = Resolver.make ~name:"inet_addr"
let sockaddr_resolver : Unix.sockaddr Resolver.resolver = Resolver.make ~name:"sockaddr"

(* And we do the association between keys and implementations. Of course, it's
   well typed again - I mean, you can not bind [inet_addr_resolver] with
   [getaddrinfo].

   Then, we have [map] with some keys associated to some resolver
   implementations - by this way, we are able to define a bucket of resolvers
   which will be used by [High.resolve] then - and do a resolution to a specific
   zone/context defined by you.

   In this case, all is available but you can use [Resolver.empty] if you want -
   server and client will not be able to do the initialization correctly. *)

let resolvers = Resolver.add inet_addr_resolver ~resolve:gethostbyname Resolver.empty
let resolvers = Resolver.add sockaddr_resolver ~resolve:getaddrinfo resolvers

let run_server_on domain =

  (* The most complex part with a side-effect too (like [Resolver.make]). We
     will register an new service which is branched with [inet_addr_resolver].
     That mean, we can branch [echo_service] only with a resolver key which must
     return a [Unix.inet_addr].

     Again, [echo_server] will not use [inet_addr_resolver], the implementation
     of the resolver. This is happen on the next line.

     Then, the user need to allocate by himself the [socket] (in this case
     [flow] is created on the [main] function). Indeed, a service __can not__
     create a flow by himself - it's like create a rabbit ex-nihilo (like the
     money).

     The point is the echo service is branched to the resolver key
     [inet_addr_resolver] - and user need to know [inet_addr_resolver] to add it
     with an implementation to the bucket of resolvers.

     This function return an abstract type [socket High.t]. *)

  let t = High.register ~name:"my echo server" inet_addr_resolver echo_service in

  (* And we resolve! What happen here? So if you remember, we describe a bucket
     of resolvers on [resolvers] - which contains at least [inet_addr_resolver]
     with the implementation [gethostbyname].

     So, because we want to resolve on [socket High.t] with [resolvers] on
     [domain], engine will call [gethostbyname] (because we associated this
     implementation with [inet_addr_resolver]), which will return a
     [Unix.inet_addr option].

     Then, we will call the [Server.init] function with the [Unix.inet_addr]
     value. And this function will return [Ok] or [Error]. Two cases are
     possible. The [init] function retrieve an error, in this case we return
     [Error (`Msg of string)], otherwise, resolver did not catch a solution for
     the [domain] (can happen if [resolvers = Resolver.empty]) and we return
     [Error `Unresolved].

     If we return [Ok] we already call [Server.init] so [t] is ready to be used
     on the echo server logic (see [server]) - because it was binded on
     [domain]. *)

  match High.resolve domain resolvers t with
  | Ok t -> server t
  | Error `Unresolved -> Fmt.invalid_arg "Unresolved domain %a.\n%!" Domain_name.pp domain
  | Error (`Msg err) -> Fmt.invalid_arg "Error: %s.\n%!" err

let run_client_on domain =

  (* Exactly the same logic on the client which will use [connect] (see
     [Client.init]) than [bind]. Again, resolution of the domain, we get a
     [Unix.sockaddr] and use it with the [Unix.connect] function.

     Then, the [Unix.file_descr] is ready to read and write something and we can
     apply the client echo logic. *)

  let t = High.register ~name:"my echo client" sockaddr_resolver client_service in
  match High.resolve domain resolvers t with
  | Ok t -> client t
  | Error `Unresolved -> Fmt.invalid_arg "Unresolved domain %a.\n%!" Domain_name.pp domain
  | Error (`Msg err) -> Fmt.invalid_arg "Error: %s.\n%!" err

let () = Sys.catch_break true

let () =

  (* As I said, it's not possible to create a flow ([Unix.inet_addr] or
     [socket]) out of the box. The user need to make them by himself outside the
     scope of [Tuyau].

     So, at the beginning, we allocate [fd] on [run_server_on] and
     [run_client_on] but I decide to move these variable on top to apply
     [Unix.close] if we get a [Ctrl-C]. Bref, voilà voilà! *)

  try
    let domain = Domain_name.of_string_exn "localhost" in

    match Unix.fork () with
    | 0 -> Unix.sleep 1 ; let _ = run_client_on domain in ()
    | _ -> let _ = run_server_on domain in ()
  with Sys.Break -> invalid_arg "You need to wait 5min."
