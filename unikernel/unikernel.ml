module Make
    (Console : Mirage_console.S)
    (Store : Mirage_kv.RO)
    (Stack : Mirage_stack.V4)
= struct
  module TCP = Tuyau_mirage_tcp.Make(Stack)
  module TLS = Tuyau_mirage_tls

  let tls_endpoint, tls_protocol = TLS.protocol_with_tls ~key:TCP.endpoint TCP.protocol
  let tls_configuration, tls_service = TLS.service_with_tls ~key:TCP.configuration TCP.service tls_protocol

  let log console fmt = Fmt.kstrf (Console.log console) fmt

  open Rresult
  open Lwt.Infix

  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

  let ( >|? ) x f = x >>= function
    | Ok x -> Lwt.return (f x)
    | Error err -> Lwt.return (Error err)

  let ( >? ) x f = Lwt.return x >>? f
  let ( <.> ) f g = fun x -> f (g x)

  let store_error err = `Store err

  let tls_server console store ~f stackv4 =
    Store.get store (Mirage_kv.Key.v "cert.pem")
    >|= R.reword_error store_error
    >|? (R.ok <.> Cstruct.of_string)
    >|? X509.Certificate.decode_pem >>? fun cert_pem ->
    Store.get store (Mirage_kv.Key.v "cert.key")
    >|= R.reword_error store_error
    >|? (R.ok <.> Cstruct.of_string)
    >|? X509.Private_key.decode_pem >>? fun (`RSA cert_key) ->
    let tls =
      Tls.Config.server
        ~certificates:(`Single ([ cert_pem ], cert_key))
        ~authenticator:(fun ~host:_ _ -> Ok None)
        () in
    let conf =
      { Tuyau_mirage_tcp.stack= stackv4
      ; keepalive= None
      ; nodelay= false
      ; port= 9090 } in
    Tuyau_mirage.impl_of_service ~key:tls_configuration tls_service >? fun (module Server) ->
    Tuyau_mirage.serve ~key:tls_configuration (conf, tls) ~service:tls_service >>? fun (t, protocol) ->

    let handle console flow () = f console flow >>= function
      | Ok () -> Lwt.return ()
      | Error (`Msg err) -> log console ">>> %s." err in
    let rec go () =
      Server.accept t >|? (R.ok <.> Tuyau_mirage.abstract protocol) >>? fun flow ->
      Lwt.async (handle console flow) ; Lwt.pause () >>= go in
    go () >|= R.reword_error (fun err -> R.msgf "%a" Server.pp_error err)

  let tcp_server console ~f stackv4 =
    let conf =
      { Tuyau_mirage_tcp.stack= stackv4
      ; keepalive= None
      ; nodelay= false
      ; port= 8080 } in
    Tuyau_mirage.impl_of_service ~key:TCP.configuration TCP.service >? fun (module Server) ->
    Tuyau_mirage.serve ~key:TCP.configuration conf ~service:TCP.service >>? fun (t, protocol) ->

    let handle console flow () = f console flow >>= function
      | Ok () -> Lwt.return ()
      | Error (`Msg err) -> log console ">>> %s." err in
    let rec go () =
      Server.accept t >|? (R.ok <.> Tuyau_mirage.abstract protocol) >>? fun flow ->
      Lwt.async (handle console flow) ; Lwt.pause () >>= go in
    go () >|= R.reword_error (fun err -> R.msgf "%a" Server.pp_error err)

  let handler console flow =
    let buf = Cstruct.create 4096 in

    let rec fully_send raw =
      Tuyau_mirage.send flow raw >>? fun len ->
      let raw = Cstruct.shift raw len in
      if Cstruct.len raw = 0
      then go ()
      else fully_send raw

    and go () =
      Tuyau_mirage.recv flow buf >>? function
      | `End_of_input -> Tuyau_mirage.close flow
      | `Input 1 ->
        if Cstruct.get_char buf 0 = '\004'
        then Tuyau_mirage.close flow
        else fully_send (Cstruct.sub buf 0 1)
      | `Input len -> fully_send (Cstruct.sub buf 0 len) in
    go () >>= function
    | Ok () -> Tuyau_mirage.close flow
    | Error err ->
      Tuyau_mirage.close flow

  let pipe flow0 flow1 =
    let buf0 = Cstruct.create 0x1000 in
    let buf1 = Cstruct.create 0x1000 in

    let rec fully_send flow raw =
      if Cstruct.len raw = 0 then Lwt.return (Ok ())
      else Tuyau_mirage.send flow raw >>? fun len ->
        fully_send flow (Cstruct.shift raw len)
    and pong () =
      Tuyau_mirage.recv flow1 buf1 >>? function
      | `End_of_input ->
        Tuyau_mirage.close flow1 >>? fun () ->
        Tuyau_mirage.close flow0
      | `Input len ->
        fully_send flow0 (Cstruct.sub buf1 0 len) >>? fun () -> ping ()
    and ping () =
      Tuyau_mirage.recv flow0 buf0 >>? function
      | `End_of_input ->
        Tuyau_mirage.close flow0 >>? fun () ->
        Tuyau_mirage.close flow1
      | `Input len ->
        fully_send flow1 (Cstruct.sub buf0 0 len) >>? fun () -> pong () in
    ping ()

  let tls_pipe console store stack resolvers ~port server =
    Store.get store (Mirage_kv.Key.v "cert.pem")
    >|= R.reword_error store_error
    >|? (R.ok <.> Cstruct.of_string)
    >|? X509.Certificate.decode_pem >>? fun cert_pem ->
    Store.get store (Mirage_kv.Key.v "cert.key")
    >|= R.reword_error store_error
    >|? (R.ok <.> Cstruct.of_string)
    >|? X509.Private_key.decode_pem >>? fun (`RSA cert_key) ->
    let tls =
      Tls.Config.server
        ~certificates:(`Single ([ cert_pem ], cert_key))
        ~authenticator:(fun ~host:_ _ -> Ok None)
        () in
    let conf =
      { Tuyau_mirage_tcp.stack
      ; keepalive= None
      ; nodelay= false
      ; port } in
    Tuyau_mirage.impl_of_service ~key:tls_configuration tls_service >? fun (module Server) ->
    Tuyau_mirage.serve ~key:tls_configuration (conf, tls) ~service:tls_service >>? fun (t, protocol) ->
    let handle console flow0 () =
      ( Tuyau_mirage.flow resolvers server >>? fun flow1 ->
        pipe flow0 flow1 ) >>= function
      | Ok () -> Lwt.return ()
      | Error err ->
        Tuyau_mirage.close flow0 >>= function
        | Ok () -> log console ">>> %a." Tuyau_mirage.pp_error err
        | Error (`Msg err) -> log console ">>> %s." err in
    let rec go () =
      Server.accept t >|? (R.ok <.> Tuyau_mirage.abstract protocol) >>? fun flow ->
      Lwt.async (handle console flow) ; Lwt.pause () >>= go in
    go () >|= R.reword_error (fun err -> R.msgf "%a" Server.pp_error err)

  let tls_domain_name = Domain_name.(host_exn <.> of_string_exn) "mirage.tls"
  let to_tls stack tls_server domain_name =
    Fmt.pr ">>> (to_tls) resolve %a.\n%!" Domain_name.pp domain_name ;
    match tls_server with
    | Some tls_server when Domain_name.equal tls_domain_name domain_name ->
      let conf = { Tuyau_mirage_tcp.stack; keepalive= None; nodelay= false; ip= tls_server; port= 9292; } in
      let tls = Tls.Config.client ~authenticator:(fun ~host:_ _ -> Ok None) () in
      Lwt.return (Some (conf, tls))
    | _ -> Lwt.return None

  let tcp_domain_name = Domain_name.(host_exn <.> of_string_exn) "mirage.tcp"
  let to_tcp stack tcp_server domain_name =
    Fmt.pr ">>> (to_tcp) resolve %a.\n%!" Domain_name.pp domain_name ;
    match tcp_server with
    | Some tcp_server when Domain_name.equal tcp_domain_name domain_name ->
      let conf = { Tuyau_mirage_tcp.stack; keepalive= None; nodelay= false; ip= tcp_server; port= 8282; } in
      Lwt.return (Some conf)
    | _ -> Lwt.return None

  let start console store stackv4 _ =
    let resolvers =
      Tuyau.empty
      |> Tuyau_mirage.register_resolver ~key:tls_endpoint (to_tls stackv4 (Key_gen.tls_server ()))
      |> Tuyau_mirage.register_resolver ~key:TCP.endpoint (to_tcp stackv4 (Key_gen.tcp_server ())) in
    let tls_server () = tls_server console store ~f:handler stackv4 >>= function
      | Ok () -> Lwt.return ()
      | Error (#Tuyau_mirage.error as err) -> log console "tls server: %a" Tuyau_mirage.pp_error err
      | Error (`Store err) -> log console "store: %a" Store.pp_error err in
    let tcp_server () = tcp_server console ~f:handler stackv4 >>= function
      | Ok () -> Lwt.return ()
      | Error (#Tuyau_mirage.error as err) -> log console "tcp server: %a" Tuyau_mirage.pp_error err in
    let tls_tls_pipe () =
      tls_pipe console store stackv4 resolvers ~port:9191 tls_domain_name >>= function
      | Ok () -> Lwt.return ()
      | Error (#Tuyau_mirage.error as err) -> log console "tls pipe to tls: %a" Tuyau_mirage.pp_error err
      | Error (`Store err) -> log console "store: %a" Store.pp_error err in
    let tls_tcp_pipe () =
      tls_pipe console store stackv4 resolvers ~port:8181 tcp_domain_name >>= function
      | Ok () -> Lwt.return ()
      | Error (#Tuyau_mirage.error as err) -> log console "tls pipe to tcp: %a" Tuyau_mirage.pp_error err
      | Error (`Store err) -> log console "store: %a" Store.pp_error err in
    Lwt.join [ tls_server ()
             ; tcp_server ()
             ; tls_tls_pipe ()
             ; tls_tcp_pipe () ]
end
