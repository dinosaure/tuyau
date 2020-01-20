type tcp =
  { socket : Unix.file_descr
  ; sockaddr : Unix.sockaddr
  ; mutable closed : bool }

module Tcp_protocol = struct
  type input = Bytes.t
  type output = String.t
  type +'a s = 'a Tuyau_unix.s

  type error = Socket_closed

  let pp_error ppf Socket_closed = Fmt.string ppf "Socket closed!"

  type endpoint = Unix.sockaddr

  type flow = tcp =
    { socket : Unix.file_descr
    ; sockaddr : Unix.sockaddr
    ; mutable closed : bool }

  let flow sockaddr =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.connect socket sockaddr ; Ok { socket; sockaddr; closed= false; }

  let rec recv ({ socket; closed; _ } as t) buf =
    if closed
    then Error Socket_closed
    else
      ( Fmt.epr "<- recv.\n%!"
      ; try let n = Unix.read socket buf 0 (Bytes.length buf) in
        if n = 0 then Ok `End_of_input
        else ( let buf = Bytes.sub buf 0 n in
               let uid = Digestif.SHA1.digest_bytes buf in
               Fmt.epr "<- recv (%a).\n%!" Digestif.SHA1.pp uid ;
               Ok (`Input buf) )
      with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> recv t buf
         | Unix.(Unix_error (EINTR, _, _)) -> recv t buf )

  let rec send ({ socket; closed; _ } as t) str =
    if closed
    then Error Socket_closed
    else
      ( let uid = Digestif.SHA1.digest_string str in
        Fmt.epr "-> send (%a).\n%!" Digestif.SHA1.pp uid ;
      try let n = Unix.write socket (Bytes.unsafe_of_string str) 0 (String.length str) in
        Ok n
      with Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) -> send t str
         | Unix.(Unix_error (EINTR, _, _)) -> send t str
         | Unix.(Unix_error (EPIPE, _, _)) ->
           t.closed <- true ; Error Socket_closed )

  let rec close t =
    try
      if not t.closed then Unix.close t.socket ;
      t.closed <- true ;
      Ok ()
    with Unix.(Unix_error (EINTR, _, _)) -> close t
end

let sockaddr
  : Unix.sockaddr Tuyau_unix.key
  = Tuyau_unix.key ~name:"tcp-unix"

let tcp_protocol = Tuyau_unix.register_protocol ~key:sockaddr ~protocol:(module Tcp_protocol)

type configuration =
  { inet_addr : Unix.inet_addr
  ; port : int
  ; capacity : int }

type master = Unix.file_descr

module Tcp_service = struct
  type +'a s = 'a Tuyau_unix.s

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  type endpoint = configuration =
    { inet_addr : Unix.inet_addr
    ; port : int
    ; capacity : int }

  type t = master

  type flow = tcp =
    { socket : Unix.file_descr
    ; sockaddr : Unix.sockaddr
    ; mutable closed : bool }

  let make { inet_addr; port; capacity; } =
    let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.bind socket (Unix.ADDR_INET (inet_addr, port)) ;
    Unix.listen socket capacity ; Ok socket

  let accept master =
    let socket, sockaddr = Unix.accept master in
    Ok { socket; sockaddr; closed= false; }
end

let configuration
  : configuration Tuyau_unix.key
  = Tuyau_unix.key ~name:"tcp-unix"

let tcp_service = Tuyau_unix.register_service ~key:configuration ~service:(module Tcp_service) ~protocol:tcp_protocol
