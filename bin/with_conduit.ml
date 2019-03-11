let () = Printexc.record_backtrace true

(* No LWT, no ASYNC ... *)

module Noop : Tuyau.Sigs.IO with type +'a t = 'a = struct
  type +'a t = 'a

  let return x = x
  let bind x f = f x
  let map f x = f x

  let ( >>= ) = bind
  let ( >|= ) x f = map f x
end

module Conduit = Tuyau.Like_conduit.Make (Noop) (Bytes)
open Conduit
open High

let conduit = Conduit.make ()

let gethostbyname domain =
  match Unix.gethostbyname (Domain_name.to_string domain) with
  | { Unix.h_addr_list= arr; _ } ->
    Fmt.pr "Resolve gethostbyname: %a (found %d solution(s)).\n%!" Domain_name.pp domain (Array.length arr) ;
    if Array.length arr > 0 then Some (`TCP (Ipaddr_unix.of_inet_addr arr.(0), 0 (* ? *))) else None

module HTTP_Server
  : Service.SERVICE with type endpoint = Tuyau.Like_conduit.endp
                     and type flow = Unix.file_descr
= struct
  type description = Service.desc
  type endpoint = Tuyau.Like_conduit.endp
  type flow = Unix.file_descr

  type buffer = Bytes.t
  type +'a io = 'a Noop.t

  type error = [ `Never ]
  type write_error = [ `Never ]

  let pp_error _ `Never = assert false
  let pp_write_error _ `Never = assert false

  let make desc endp = match endp with
    | `TCP (ip, port) ->
      let inet_addr = Ipaddr_unix.to_inet_addr ip in
      let socket = Unix.socket Unix.PF_INET SOCK_STREAM 0 in
      Fmt.pr "Server %s start to listen.\n%!" desc.Service.name ;
      Unix.bind socket (Unix.ADDR_INET (inet_addr, port)) ;
      Unix.listen socket 40 (* get it from [inet_addr] or [socket]? *) ; Ok socket
    | _ -> Fmt.invalid_arg "Implementation can not handle others endpoints"

  let read socket =
    let bytes = Bytes.create 0x100 in
    let n = Unix.read socket bytes 0 (Bytes.length bytes) in
    if n = 0 then Ok `Eoi else Ok (`Data (Bytes.sub bytes 0 n))

  let write socket bytes = let n = Unix.write socket bytes 0 (Bytes.length bytes) in Ok n
  let close socket = let () = Unix.close socket in Ok ()
end

module HTTPS_Server
  : Service.SERVICE with type endpoint = Tuyau.Like_conduit.endp
                     and type flow = Unix.file_descr
= struct
  type description = Service.desc
  type endpoint = Tuyau.Like_conduit.endp
  type flow = Unix.file_descr

  type buffer = Bytes.t
  type +'a io = 'a Noop.t

  type error = [ `Never ]
  type write_error = [ `Never ]

  let pp_error _ `Never = assert false
  let pp_write_error _ `Never = assert false

  let make desc endp = match endp with
    | `TLS (certificate, `TCP (ip, port)) ->
      let inet_addr = Ipaddr_unix.to_inet_addr ip in
      let socket = Unix.socket Unix.PF_INET SOCK_STREAM 0 in
      Fmt.pr "Server %s (with certificate %s) start to listen.\n%!" desc.Service.name certificate ;
      Unix.bind socket (Unix.ADDR_INET (inet_addr, port)) ;
      Unix.listen socket 40 (* get it from [inet_addr] or [socket]? *) ; Ok socket
    | _ -> Fmt.invalid_arg "Implementation can not handle others endpoints"

  let read socket =
    let bytes = Bytes.create 0x100 in
    let n = Unix.read socket bytes 0 (Bytes.length bytes) in
    if n = 0 then Ok `Eoi else Ok (`Data (Bytes.sub bytes 0 n))

  let write socket bytes = let n = Unix.write socket bytes 0 (Bytes.length bytes) in Ok n
  let close socket = let () = Unix.close socket in Ok ()
end

let http_service = Service.of_module ~name:"http" ~port:80 ~kind:Service.TCP (module HTTP_Server)
let http_scheme = Tuyau.Scheme.of_string_exn "http"

let https_service = Service.of_module ~name:"https" ~port:443 ~kind:Service.TCP (module HTTPS_Server)
let https_scheme = Tuyau.Scheme.of_string_exn "https"

let gethostbyname_with_cert domain =
  match gethostbyname domain with
  | Some endp ->
    Some (`TLS ("cert0", endp))
  | x -> x

let resolver_with_cert = Resolver.make ~name:"with certificate"

let () = Conduit.register_scheme http_scheme (High.register ~name:"http" Conduit.Resolver.default http_service)
let () = Conduit.register_scheme https_scheme (High.register ~name:"https" resolver_with_cert https_service)

let () =
  let resolver =
    let open Conduit.Resolver in
    empty
    |> add default ~resolve:gethostbyname
    |> add resolver_with_cert ~resolve:gethostbyname_with_cert in
  match Conduit.resolve ~uri:(Uri.of_string "http://localhost/") ~resolver conduit,
        Conduit.resolve ~uri:(Uri.of_string "https://localhost/") ~resolver conduit with
  | Ok (Conduit.S h0, _), Ok (Conduit.S h1, _) ->
    let description0 = Conduit.High.Service.service_of_scheme (Conduit.High.witness h0) in
    let description1 = Conduit.High.Service.service_of_scheme (Conduit.High.witness h1) in
    Fmt.pr "%s service loaded.\n%!" description0.Conduit.High.Service.name ;
    Fmt.pr "%s service loaded.\n%!" description1.Conduit.High.Service.name
  | _ -> assert false
