module Make
    (Console : Mirage_console.S)
    (Store : Mirage_kv.RO)
    (Stack : Mirage_stack.V4)
= struct
  module TCP = Tuyau_mirage_tcp.Make(Stack)
  module TLS = Tuyau_tls.Make(Tuyau_mirage.Mirage_scheduler)(Tuyau_mirage)

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

  let start console store stackv4 _ =
    let tls_server () = tls_server console store ~f:handler stackv4 >>= function
      | Ok () -> Lwt.return ()
      | Error (#Tuyau_mirage.error as err) -> log console "tls server: %a" Tuyau_mirage.pp_error err
      | Error (`Store err) -> log console "store: %a" Store.pp_error err in
    let tcp_server () = tcp_server console ~f:handler stackv4 >>= function
      | Ok () -> Lwt.return ()
      | Error (#Tuyau_mirage.error as err) -> log console "tcp server: %a" Tuyau_mirage.pp_error err in
    Lwt.join [ tls_server (); tcp_server () ]
end
