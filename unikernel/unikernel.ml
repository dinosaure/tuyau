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
        ~authenticator:X509.Authenticator.null
        () in
    let conf =
      { Tuyau_mirage_tcp.stack= stackv4
      ; keepalive= None
      ; port= 9090 } in
    Tuyau_mirage.server ~key:tls_configuration tls_service >? fun (module Server) ->
    Tuyau_mirage.service ~key:tls_configuration (conf, tls) ~service:tls_service >>? fun (t, protocol) ->

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
      ; port= 8080 } in
    Tuyau_mirage.server ~key:TCP.configuration TCP.service >? fun (module Server) ->
    Tuyau_mirage.service ~key:TCP.configuration conf ~service:TCP.service >>? fun (t, protocol) ->

    let handle console flow () = f console flow >>= function
      | Ok () -> Lwt.return ()
      | Error (`Msg err) -> log console ">>> %s." err in
    let rec go () =
      Server.accept t >|? (R.ok <.> Tuyau_mirage.abstract protocol) >>? fun flow ->
      Lwt.async (handle console flow) ; Lwt.pause () >>= go in
    go () >|= R.reword_error (fun err -> R.msgf "%a" Server.pp_error err)

  let handler console (Tuyau_mirage.Flow (flow, (module Flow))) =
    let buf = Cstruct.create 4096 in
    let reword_error = R.reword_error (fun err -> R.msgf "%a" Flow.pp_error err) in

    let rec fully_send raw =
      Flow.send flow raw >>? fun len ->
      let raw = Cstruct.shift raw len in
      if Cstruct.len raw = 0
      then go ()
      else fully_send raw

    and go () =
      Flow.recv flow buf >>? function
      | `End_of_input -> Flow.close flow
      | `Input 1 ->
        if Cstruct.get_char buf 0 = '\004'
        then Flow.close flow
        else fully_send (Cstruct.sub buf 0 1)
      | `Input len -> fully_send (Cstruct.sub buf 0 len) in
    go () >>= function
    | Ok () -> Flow.close flow >|= reword_error
    | Error err ->
      Flow.close flow >|= reword_error >>? fun () ->
      Lwt.return (R.error_msgf "%a" Flow.pp_error err)

  let pipe (Tuyau_mirage.Flow (flow0, (module Flow0)) as f0) (Tuyau_mirage.Flow (flow1, (module Flow1)) as f1) =
    let buf0 = Cstruct.create 0x1000 in
    let buf1 = Cstruct.create 0x1000 in
    let reword0 = R.msgf "%a" Flow0.pp_error in
    let reword1 = R.msgf "%a" Flow1.pp_error in

    let rec fully_send (Tuyau_mirage.Flow (flow, (module Flow)) as f) raw =
      if Cstruct.len raw = 0 then Lwt.return (Ok ())
      else Flow.send flow raw >|= R.reword_error (R.msgf "%a" Flow.pp_error) >>? fun len ->
        fully_send f (Cstruct.shift raw len)
    and pong () =
      Flow1.recv flow1 buf1 >|= R.reword_error reword1 >>? function
      | `End_of_input ->
        Flow1.close flow1 >|= R.reword_error reword1 >>? fun () ->
        Flow0.close flow0 >|= R.reword_error reword0
      | `Input len ->
        fully_send f0 (Cstruct.sub buf1 0 len) >>? fun () -> ping ()
    and ping () =
      Flow0.recv flow0 buf0 >|= R.reword_error reword0 >>? function
      | `End_of_input ->
        Flow0.close flow0 >|= R.reword_error reword0 >>? fun () ->
        Flow1.close flow1 >|= R.reword_error reword1
      | `Input len ->
        fully_send f1 (Cstruct.sub buf0 0 len) >>? fun () -> pong () in
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
        ~authenticator:X509.Authenticator.null
        () in
    let conf =
      { Tuyau_mirage_tcp.stack
      ; keepalive= None
      ; port } in
    Tuyau_mirage.server ~key:tls_configuration tls_service >? fun (module Server) ->
    Tuyau_mirage.service ~key:tls_configuration (conf, tls) ~service:tls_service >>? fun (t, protocol) ->
    let handle console (Tuyau_mirage.Flow (flow0, (module Flow0)) as f0) () =
      ( Tuyau_mirage.flow resolvers server >|? (R.ok <.> Tuyau_mirage.unlift) >>? fun f1 ->
        pipe f0 f1 ) >>= function
      | Ok () -> Lwt.return ()
      | Error err ->
        Flow0.close flow0 >|= R.reword_error (R.msgf "%a" Flow0.pp_error) >>= function
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
      let conf = { Tuyau_mirage_tcp.stack; keepalive= None; ip= tls_server; port= 9292; } in
      let tls = Tls.Config.client ~authenticator:X509.Authenticator.null () in
      Lwt.return (Some (conf, tls))
    | _ -> Lwt.return None

  let tcp_domain_name = Domain_name.(host_exn <.> of_string_exn) "mirage.tcp"
  let to_tcp stack tcp_server domain_name =
    Fmt.pr ">>> (to_tcp) resolve %a.\n%!" Domain_name.pp domain_name ;
    match tcp_server with
    | Some tcp_server when Domain_name.equal tcp_domain_name domain_name ->
      let conf = { Tuyau_mirage_tcp.stack; keepalive= None; ip= tcp_server; port= 8282; } in
      Lwt.return (Some conf)
    | _ -> Lwt.return None

  let start console store stackv4 _ =
    let resolvers =
      Tuyau_mirage.Map.empty
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
