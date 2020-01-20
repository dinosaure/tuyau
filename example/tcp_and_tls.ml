module Tuyau_tls = Tuyau_tls.Make(Tuyau_unix.Unix_scheduler)(Tuyau_unix)

let ( <.> ) f g = fun x -> f (g x)

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

let sockaddr_and_tls, tcp_protocol_and_tls = Tuyau_tls.flow_with_tls ~key:sockaddr tcp_protocol
let config_and_tls, tcp_service_and_tls = Tuyau_tls.service_with_tls ~key:configuration tcp_service tcp_protocol_and_tls

let tcp_config =
  { inet_addr= Unix.inet_addr_any
  ; port= 4242
  ; capacity= 40 }

let server () =
  let open Rresult in
  Tuyau_unix.service
    ~key:config_and_tls (tcp_config, tls_config)
    ~service:tcp_service_and_tls >>= fun (master, w) ->
  Tuyau_unix.server ~key:config_and_tls tcp_service_and_tls >>= fun (module Server) ->
  let rec loop master =
    Server.accept master
    |> R.reword_error (fun err -> R.msgf "%a" Server.pp_error err)
    >>= fun flow ->
    Tuyau_unix.lift flow w >>| Tuyau_unix.unlift >>= fun (Tuyau_unix.Flow (socket, (module Flow))) ->
    let buf = Bytes.create 0x1000 in
    let rec go () =
      Flow.recv socket buf >>= function
      | `End_of_input ->
        Flow.close socket
      | `Input buf ->
        Flow.send socket (Bytes.to_string buf) >>= fun _len ->
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
    Some (Unix.ADDR_INET (inet_addr, port), `As_client tls)

let client ?(port= 4242) domain_name =
  let resolvers = Tuyau_unix.register_resolver ~key:sockaddr_and_tls (resolver ~port) Tuyau_unix.Map.empty in
  let open Rresult in
  Tuyau_unix.flow resolvers domain_name >>= fun flow ->
  Tuyau_unix.unlift flow |> fun (Tuyau_unix.Flow (socket, (module Flow))) ->
  let rec go () = match input_line stdin with
    | line ->
      let buf = Bytes.create (String.length line) in
      Flow.send socket line >>= fun _ ->
      ( Flow.recv socket buf >>= function
      | `End_of_input -> Flow.close socket
      | `Input buf ->
        assert (Bytes.unsafe_to_string buf = line) ;
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
