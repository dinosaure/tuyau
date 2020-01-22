type ('stack, 'ip) endpoint =
  { stack : 'stack
  ; keepalive : Mirage_protocols.Keepalive.t option
  ; ip : 'ip
  ; port : int }

type 'stack configuration =
  { stack : 'stack
  ; keepalive : Mirage_protocols.Keepalive.t option
  ; port : int }

module Make (StackV4 : Mirage_stack.V4)
  : Tuyau_mirage.T
    with type endpoint = (StackV4.t, Ipaddr.V4.t) endpoint
     and type configuration = StackV4.t configuration
