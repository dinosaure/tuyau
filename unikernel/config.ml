open Mirage

let unikernel =
  foreign "Unikernel.Make"
    ~deps:[ abstract nocrypto ]
    (console @-> kv_ro @-> stackv4 @-> job)

let packages =
  [ package "tuyau" ~sublibs:[ "tls"; "mirage"; "mirage.tcp"; ] ]

let default_stackv4 = generic_stackv4 default_network
let store = generic_kv_ro "cert"

let () =
  register "unikernel"
    ~packages
    [ unikernel $ default_console $ store $ default_stackv4 ]
