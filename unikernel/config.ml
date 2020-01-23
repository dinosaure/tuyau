open Mirage

let tls_server =
  let doc = Key.Arg.info ~doc:"TLS server IP" [ "tls" ] in
  Key.(create "tls-server" Arg.(opt (some ipv4_address) None doc))

let tcp_server =
  let doc = Key.Arg.info ~doc:"TCP server IP" [ "tcp" ] in
  Key.(create "tcp-server" Arg.(opt (some ipv4_address) None doc))

let unikernel =
  foreign "Unikernel.Make"
    ~deps:[ abstract nocrypto ]
    ~keys:[ Key.abstract tls_server; Key.abstract tcp_server ]
    (console @-> kv_ro @-> stackv4 @-> job)

let packages =
  [ package "tuyau" ~sublibs:[ "mirage"; "mirage.tcp"; "mirage.tls"; ] ]

let store = generic_kv_ro "cert"

let () =
  let store = generic_kv_ro "cert" in
  let network = generic_stackv4 default_network in
  register "unikernel"
    ~packages
    [ unikernel $ default_console $ store $ network ]
