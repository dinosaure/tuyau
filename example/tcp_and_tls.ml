module Tuyau_tls = Tuyau_tls.Make(Tuyau_unix.Unix_scheduler)(Tuyau_unix)

let ( <.> ) f g = fun x -> f (g x)
let reporter = Logs_fmt.reporter ()
let () = Logs.set_reporter reporter
let () = Logs.set_level ~all:true (Some Logs.Debug)

(* TLS preamble. *)

let () = Nocrypto_entropy_unix.initialize ()

let load_file filename =
  let open Rresult in
  Bos.OS.File.read filename >>= fun contents ->
  R.ok (Cstruct.of_string contents)

let cert =
  let open Rresult in
  load_file (Fpath.v "server.pem") >>= fun raw ->
  X509.Certificate.decode_pem raw

let cert = Rresult.R.get_ok cert

let private_key =
  let open Rresult in
  load_file (Fpath.v "server.key") >>= fun raw ->
  X509.Private_key.decode_pem raw >>= fun (`RSA v) -> R.ok v

let private_key = Rresult.R.get_ok private_key

let tls_config =
  Tls.Config.server
    ~certificates:(`Single ([ cert ], private_key))
    ~authenticator:(fun ~host:_ _ -> Ok None)
    ()

open Tuyau_unix_tcp

let tls_endpoint, tls_protocol = Tuyau_tls.protocol_with_tls ~key:Tuyau_unix_tcp.endpoint Tuyau_unix_tcp.protocol
let tls_configuration, tls_service = Tuyau_tls.service_with_tls ~key:configuration Tuyau_unix_tcp.service tls_protocol

(* Usual configuration for a TCP server. *)

let tcp_config =
  { inet_addr= Unix.inet_addr_any
  ; port= 4242
  ; capacity= 40 }

let server () =
  let open Rresult in
  Tuyau_unix.serve
    ~key:tls_configuration (tcp_config, tls_config)
    ~service:tls_service >>= fun (master, protocol) ->
  Tuyau_unix.impl_of_service ~key:tls_configuration tls_service >>= fun (module Server) ->
  let rec loop master =
    Server.accept master
    |> R.reword_error (fun err -> R.msgf "%a" Server.pp_error err)
    >>| Tuyau_unix.abstract protocol
    >>= fun flow ->
    let raw = Cstruct.create 0x1000 in
    let rec go () =
      Tuyau_unix.recv flow raw >>= function
      | `End_of_input ->
        Tuyau_unix.close flow
      | `Input len ->
        Tuyau_unix.send flow (Cstruct.sub raw 0 len) >>= fun _len ->
        go () in
    go () >>= fun () -> loop master in
  loop master

let resolver ~port domain =
  let { Unix.h_addr_list; _ } = Unix.gethostbyname (Domain_name.to_string domain) in
  if Array.length h_addr_list = 0
  then None
  else
    let inet_addr = h_addr_list.(0) in
    let tls = Tls.Config.client ~authenticator:(fun ~host:_ _ -> Ok None) () in
    Some (Unix.ADDR_INET (inet_addr, port), tls)

let client ?(port= 4242) domain_name =
  let open Rresult in
  let resolvers = Tuyau_unix.register_resolver ~key:tls_endpoint (resolver ~port) Tuyau.empty in
  Tuyau_unix.flow resolvers domain_name >>= fun flow ->
  let raw0 = Cstruct.create 0x1000 in
  let rec go () = match input_line stdin with
    | line ->
      let raw1 =
        Cstruct.of_string
          ~allocator:(fun len -> if len > Cstruct.len raw0 then Fmt.invalid_arg "Input line too long." ; Cstruct.sub raw0 0 len)
          line in
      Tuyau_unix.send flow raw1 >>= fun _ ->
      ( Tuyau_unix.recv flow raw0 >>= function
      | `End_of_input -> Tuyau_unix.close flow
      | `Input bytes ->
        Fmt.epr "<~ %S.\n%!" Cstruct.(to_string (sub raw0 0 bytes)) ;
        go () )
    | exception End_of_file ->
      Tuyau_unix.close flow in
  go ()

let run () = match Sys.argv with
  | [| _; "server" |] -> server ()
  | [| _; "client"; domain_name |] ->
    let domain_name = Domain_name.(host_exn <.> of_string_exn) domain_name in
    client domain_name
  | [| _; "client"; domain_name; port; |] ->
    let port = int_of_string port in
    let domain_name = Domain_name.(host_exn <.> of_string_exn) domain_name in
    client ~port domain_name
  | _ -> Rresult.R.error_msgf "%s (server|client host port)" Sys.argv.(0)

let () = match run () with
  | Ok () -> ()
  | Error err -> Fmt.epr "[ERROR]: %a.\n%!" Tuyau_unix.pp_error err
