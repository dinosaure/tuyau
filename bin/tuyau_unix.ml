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
  : Service.SERVICE with type endpoint = Unix.inet_addr
                     and type flow = Unix.file_descr
= struct
  type description = Service.desc
  type endpoint = Unix.inet_addr
  type flow = Unix.file_descr

  type buffer = Bytes.t
  type +'a io = 'a Noop.t

  type error = [ `Never ]
  type write_error = [ `Never ]

  let pp_error _ `Never = assert false
  let pp_write_error _ `Never = assert false

  let init desc inet_addr fd =
    Fmt.pr "Server start to listen.\n%!" ;
    Unix.bind fd (Unix.ADDR_INET (inet_addr, desc.Service.port)) ;
    Unix.listen fd 40 ; Ok fd

  let read fd =
    let bytes = Bytes.create 0x100 in
    let n = Unix.read fd bytes 0 (Bytes.length bytes) in
    if n = 0 then Ok `Eoi else Ok (`Data (Bytes.sub bytes 0 n))

  let write fd bytes = let n = Unix.write fd bytes 0 (Bytes.length bytes) in Ok n
  let close fd = let () = Unix.close fd in Ok ()
end

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

  let init desc sockaddr fd =
    Fmt.pr "Client start to connect to %a.\n%!" pp_sockaddr sockaddr ;
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

let echo_service = Service.of_module ~name:"echo" ~port:8080 ~kind:Service.TCP (module Server)
let client_service = Service.of_module ~name:"client echo" ~port:8080 ~kind:Service.TCP (module Client)

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

let pp_char ppf = function
  | '\032' .. '\126' as chr -> Fmt.char ppf chr
  | x -> Fmt.pf ppf "%02x" (Char.code x)

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
  High.extract t (* extract action *) @@ fun master impl ->
  let rec loop : unit -> unit = fun () ->
    let conn = Unix.accept master in
    let () = handle_connection impl conn in
    loop () in
  loop () ; master

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

let run_server_on flow domain =
  let t = High.register ~name:"my echo server" inet_addr_resolver echo_service flow in
  match High.resolve domain resolvers t with
  | Ok t -> server t
  | Error `Unresolved -> Fmt.invalid_arg "Unresolved domain %a.\n%!" Domain_name.pp domain
  | Error (`Msg err) -> Fmt.invalid_arg "Error: %s.\n%!" err

let run_client_on flow domain =
  let t = High.register ~name:"my echo client" sockaddr_resolver client_service flow in
  match High.resolve domain resolvers t with
  | Ok t -> client t
  | Error `Unresolved -> Fmt.invalid_arg "Unresolved domain %a.\n%!" Domain_name.pp domain
  | Error (`Msg err) -> Fmt.invalid_arg "Error: %s.\n%!" err

let () = Sys.catch_break true

let () =
  let server = Unix.socket Unix.PF_INET SOCK_STREAM 0 in
  let client = Unix.socket Unix.PF_INET SOCK_STREAM 0 in

  try
    let domain = Domain_name.of_string_exn "localhost" in

    match Unix.fork () with
    | 0 -> Unix.sleep 1 ; let _ = run_client_on client domain in ()
    | _ -> let _ = run_server_on server domain in ()
  with Sys.Break ->
    Unix.close server ;
    Unix.close client
