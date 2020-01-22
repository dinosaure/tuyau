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
  load_file (Fpath.v "ptt.pem") >>= fun raw ->
  X509.Certificate.decode_pem raw

let cert = Rresult.R.get_ok cert

let private_key =
  let open Rresult in
  load_file (Fpath.v "ptt.key") >>= fun raw ->
  X509.Private_key.decode_pem raw >>= fun (`RSA v) -> R.ok v

let private_key = Rresult.R.get_ok private_key

let tls_config =
  Tls.Config.server
    ~certificates:(`Single ([ cert ], private_key))
    ~authenticator:X509.Authenticator.null
    ()

open Tuyau_unix_tcp

(* [Tuyau_unix_tcp] provides 4 things:
   - a [sockaddr] type witness to let the user to define a way
     to resolve a [Domain_name.t] to give to use a [Unix.sockaddr]
   - a [tcp_protocol] type witness which represents an implementation
     of the TCP protocol
   - a [configuration] type witness to let the user to _configure_
     a TCP service (inet_addr and port, where we bind the server and capacity about [Unix.listen])
   - a [tcp_service] type witness which serves TCP connection *)

(* We do the composition between the implementation of the TCP protocol
   and the TCP service. COMPOSITION!!! *)
let sockaddr_and_tls, tcp_protocol_and_tls = Tuyau_tls.protocol_with_tls ~key:Tuyau_unix_tcp.endpoint Tuyau_unix_tcp.protocol
let config_and_tls, tcp_service_and_tls = Tuyau_tls.service_with_tls ~key:configuration Tuyau_unix_tcp.service tcp_protocol_and_tls

(* Usual configuration for a TCP server. *)

let tcp_config =
  { inet_addr= Unix.inet_addr_any
  ; port= 4242
  ; capacity= 40 }

let server () =
  let open Rresult in
  Tuyau_unix.service
    ~key:config_and_tls (tcp_config, tls_config)
    ~service:tcp_service_and_tls >>= fun (master, protocol) ->
  (* - [master] is the master socket
     - [w] is the type witness of the protocol *)
  Tuyau_unix.server ~key:config_and_tls tcp_service_and_tls >>= fun (module Server) ->
  let rec loop master =
    (* for each [accept], we should start a thread to be able to
       handle multiple connection. *)
    Server.accept master
    |> R.reword_error (fun err -> R.msgf "%a" Server.pp_error err)
    >>| Tuyau_unix.abstract protocol
    >>= fun (Tuyau_unix.Flow (socket, (module Flow))) ->
    (* at this stage, we can choose 2 ways to handle the given socket.
       - we know, by types, that [flow : Tuyau_unix_tcp.tcpi flow_with_tls] and functions on it don't exist.
         Currently, the type is abstracted. However, we can expose definition of it and, in this context,
         directly uses functions associated by this type (like [read] and [write] - but they don't exist).
       - we prefer to trust API provided by [w] (the type witness of the protocol) which handles correctly
         TLS things correctly.

       The idea behind all of that is to say that we can completely have something which is abstracted
       and still be able to do the composition - and the user should not know anything about underlying
       structures. But we can choose to expose the composition and internal structures and the user will be able
       to tweak them then. *)
    let raw = Cstruct.create 0x1000 in
    let rec go () =
      Flow.recv socket raw >>= function
      | `End_of_input ->
        Flow.close socket
      | `Input len ->
        Flow.send socket (Cstruct.sub raw 0 len) >>= fun _len ->
        go () in
    go () |> R.reword_error (fun err -> R.msgf "%a" Flow.pp_error err)
    >>= fun () -> loop master in
  loop master

let resolver ~port domain =
  let { Unix.h_addr_list; _ } = Unix.gethostbyname (Domain_name.to_string domain) in
  if Array.length h_addr_list = 0
  then ( Fmt.epr ">>> %a not found.\n%!" Domain_name.pp domain ; None )
  else
    let inet_addr = h_addr_list.(0) in
    let tls = Tls.Config.client ~authenticator:X509.Authenticator.null () in
    Some (Unix.ADDR_INET (inet_addr, port), tls)

let client ?(port= 4242) domain_name =
  let resolvers = Tuyau_unix.register_resolver ~key:sockaddr_and_tls (resolver ~port) Tuyau_unix.Map.empty in
  let open Rresult in
  Tuyau_unix.flow resolvers domain_name >>= fun flow ->
  Tuyau_unix.unlift flow |> fun (Tuyau_unix.Flow (socket, (module Flow))) ->
  let raw0 = Cstruct.create 0x1000 in
  let rec go () = match input_line stdin with
    | line ->
      let raw1 =
        Cstruct.of_string
          ~allocator:(fun len -> if len > Cstruct.len raw0 then Fmt.invalid_arg "Input line too long." ; Cstruct.sub raw0 0 len)
          line in
      Flow.send socket raw1 >>= fun _ ->
      ( Flow.recv socket raw0 >>= function
      | `End_of_input -> Flow.close socket
      | `Input bytes ->
        Fmt.epr "<~ %S.\n%!" Cstruct.(to_string (sub raw0 0 bytes)) ;
        go () )
    | exception End_of_file ->
      Flow.close socket in
  go () |> R.reword_error (fun err -> R.msgf "%a" Flow.pp_error err)

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
