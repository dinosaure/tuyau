let () = Printexc.record_backtrace true

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

module Server
  : Service.SERVICE with type from = Unix.inet_addr
                     and type endpoint = Unix.file_descr
= struct
  type description = Service.desc
  type endpoint = Unix.file_descr
  type from = Unix.inet_addr

  type buffer = Bytes.t
  type +'a io = 'a Noop.t

  let init desc inet_addr fd =
    Fmt.pr "Server start to listen.\n%!" ;
    Unix.bind fd (Unix.ADDR_INET (inet_addr, desc.Service.port)) ;
    Unix.listen fd 40 ; fd

  let read fd bytes = Unix.read fd bytes 0 (Bytes.length bytes)
  let write fd bytes = Unix.write fd bytes 0 (Bytes.length bytes)
end

module Client = struct
  type description = Service.desc
  type endpoint = Unix.file_descr
  type from = Unix.sockaddr

  type buffer = Bytes.t
  type +'a io = 'a Noop.t

  let pp_inet_addr ppf _ = Fmt.string ppf "#inet-addr"

  let pp_sockaddr ppf = function
    | Unix.ADDR_UNIX service -> Fmt.pf ppf "<%s>" service
    | Unix.ADDR_INET (inet_addr, port) -> Fmt.pf ppf "%a:%d" pp_inet_addr inet_addr port

  let init desc sockaddr fd =
    Fmt.pr "Client start to connect to %a.\n%!" pp_sockaddr sockaddr ;
    let with_port = function
      | Unix.ADDR_UNIX _ as v -> v
      | Unix.ADDR_INET (inet_addr, _) -> Unix.ADDR_INET (inet_addr, desc.Service.port) in
    Unix.connect fd (with_port sockaddr) ; fd

  let read fd bytes = Unix.read fd bytes 0 (Bytes.length bytes)
  let write fd bytes = Unix.write fd bytes 0 (Bytes.length bytes)
end

let echo_service = Service.of_module ~name:"echo" ~port:8080 ~kind:Service.TCP (module Server)
let client_service = Service.of_module ~name:"client echo" ~port:8080 ~kind:Service.TCP (module Client)

let read_line action fd =
  let one = Bytes.create 1 in
  let buf = Buffer.create 16 in
  let rec fill () =
    let n = action.Service.rd fd one in
    match n, Bytes.get one 0 with
    | 0, _ -> `Eoi (Buffer.contents buf)
    | _, '\n' -> `Ok (Buffer.contents buf)
    | _, chr -> Buffer.add_char buf chr ; fill () in
  fill ()

let handle_message action fd =
  let read_line = read_line action in

  let rec go () = match read_line fd with
    | `Eoi trailer -> Fmt.pr "Client gave up with %s.\n%!" trailer
    | `Ok "ping" ->
      Fmt.pr "Server> receive ping.\n%!" ;
      let _ = action.Service.wr fd (Bytes.of_string "pong\n") in go ()
    | `Ok response ->
      let _ = action.Service.wr fd (Bytes.of_string response) in
      Fmt.pr "Unsupported response: %s.\n%!" response ;
      go () in
  go ()

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

let inet_addr_resolver : Unix.inet_addr Resolver.resolver = Resolver.make ~name:"inet_addr"
let sockaddr_resolver : Unix.sockaddr Resolver.resolver = Resolver.make ~name:"sockaddr"

let resolvers = Resolver.add inet_addr_resolver ~resolve:gethostbyname Resolver.empty
let resolvers = Resolver.add sockaddr_resolver ~resolve:getaddrinfo resolvers

let handle_connection action conn =
  let fd, _ = conn in
  handle_message action fd

let server t =
  High.map t (* extract fd *) @@ fun master ->
  High.action t (* extract action *) @@ fun action ->
  let rec loop : unit -> unit = fun () ->
    let conn = Unix.accept master in
    let () = handle_connection action conn in
    loop () in
  loop () ; master

let client t =
  High.map t @@ fun fd ->
  High.action t @@ fun action ->
  let read_line = read_line action in
  let rec loop : unit -> unit = fun () ->
    let _ = action.Service.wr fd (Bytes.of_string "ping\n") in
    match read_line fd with
    | `Eoi trailer -> Fmt.pr "Server gave up with: %s.\n%!" trailer
    | `Ok "pong" -> Fmt.pr "Client> receive pong.\n%!" ; loop ()
    | `Ok response ->
      Fmt.pr "Unsupported response: %s.\n%!" response ;
      loop () in
  loop () ; fd

let run_server_on domain =
  let endpoint = Unix.socket Unix.PF_INET SOCK_STREAM 0 in
  let t = High.make ~name:"my echo server" inet_addr_resolver echo_service endpoint in
  match High.resolve domain resolvers t with
  | Some t -> server t
  | None -> Fmt.invalid_arg "domain %a not found" Domain_name.pp domain

let run_client_on domain =
  let endpoint = Unix.socket Unix.PF_INET SOCK_STREAM 0 in
  let t = High.make ~name:"my echo client" sockaddr_resolver client_service endpoint in
  match High.resolve domain resolvers t with
  | Some t -> client t
  | None -> Fmt.invalid_arg "domain %a not found" Domain_name.pp domain

let () =
  let domain = Domain_name.of_string_exn "localhost" in

  match Unix.fork () with
  | 0 -> Unix.sleep 1 ; let _ = run_client_on domain in ()
  | _ -> let _ = run_server_on domain in ()
