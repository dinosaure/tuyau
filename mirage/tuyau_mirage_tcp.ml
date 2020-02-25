module Ke = Ke.Rke.Weighted

type ('stack, 'ip) endpoint =
  { stack : 'stack
  ; keepalive : Mirage_protocols.Keepalive.t option
  ; ip : 'ip
  ; port : int }

type 'stack configuration =
  { stack : 'stack
  ; keepalive : Mirage_protocols.Keepalive.t option
  ; port : int }

module Make (StackV4 : Mirage_stack.V4) = struct
  open Rresult
  open Lwt.Infix

  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> Lwt.return (Error err)

  let src = Logs.Src.create "tuyau-mirage-tcpip"
  module Log = (val Logs.src_log src : Logs.LOG)

  type protocol =
    { flow : StackV4.TCPV4.flow
    ; queue : (char, Bigarray.int8_unsigned_elt) Ke.t }

  let dst { flow; _ } = StackV4.TCPV4.dst flow

  type nonrec endpoint = (StackV4.t, Ipaddr.V4.t) endpoint

  module Protocol = struct
    type input = Tuyau_mirage.input
    type output = Tuyau_mirage.output
    type +'a s = 'a Tuyau_mirage.s

    type error =
      | Input_too_large
      | TCP_error of StackV4.TCPV4.error
      | Write_error of StackV4.TCPV4.write_error

    let pp_error ppf = function
      | Input_too_large -> Fmt.string ppf "Input too large"
      | TCP_error err -> StackV4.TCPV4.pp_error ppf err
      | Write_error err -> StackV4.TCPV4.pp_write_error ppf err

    let error : StackV4.TCPV4.error -> error = fun err -> TCP_error err
    let write_error : StackV4.TCPV4.write_error -> error = fun err -> Write_error err

    type flow = protocol =
      { flow : StackV4.TCPV4.flow
      ; queue : (char, Bigarray.int8_unsigned_elt) Ke.t }

    type nonrec endpoint = endpoint

    let flow { stack; keepalive; ip; port; }=
      let tcpv4 = StackV4.tcpv4 stack in
      StackV4.TCPV4.create_connection tcpv4 ?keepalive (ip, port) >|= R.reword_error error >>? fun flow ->
      let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
      Lwt.return (Ok { flow; queue; })

    let length = Cstruct.len

    let blit src src_off dst dst_off len =
      let src = Cstruct.to_bigarray src in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len

    let recv t raw =
      match Ke.N.peek t.queue with
      | [] ->
        Log.debug (fun m -> m "<- Read the TCP flow.") ;
        ( StackV4.TCPV4.read t.flow >|= R.reword_error error >>? function
        | `Eof ->
          Log.debug (fun m -> m "<- End of input.") ;
          Lwt.return (Ok `End_of_input)
        | `Data buf ->
          Log.debug (fun m -> m "<- Got %d byte(s)." (Cstruct.len buf)) ;
          (* XXX(dinosaure): [telnet] send '\004' (End Of Transmission) to ask
             the service to close the connection. [mirage-tcpip] does not handle
             this _opcode_ so we handle it in this place. *)
          if Cstruct.len buf = 1 && Cstruct.get_char buf 0 = '\004'
          then ( StackV4.TCPV4.close t.flow >>= fun () ->
                 Log.debug (fun m -> m "<- End of input (end of transmission)") ;
                 Lwt.return (Ok `End_of_input))
          else
            ( let max_buf = Cstruct.len buf in
              let max_raw = Cstruct.len raw in
              if max_buf <= max_raw
              then
                ( Cstruct.blit buf 0 raw 0 max_buf
                ; Lwt.return (Ok (`Input max_buf)) )
              else
                ( Cstruct.blit buf 0 raw 0 max_raw
                ; let len = min (max_buf - max_raw) (Ke.available t.queue) in
                  Log.debug (fun m -> m "<- Save %d into the queue." len) 
                ; let _ = Ke.N.push_exn ~blit ~length ~off:max_raw ~len t.queue buf in
                  if len = (max_buf - max_raw)
                  then Lwt.return (Ok (`Input max_raw))
                  else Lwt.return (Error Input_too_large) ) ) )
      | lst ->
        let rec go consumed raw = function
          | [] ->
            Log.debug (fun m -> m "<- Shift %d bytes." consumed) ;
            Ke.N.shift_exn t.queue consumed ;

            (* XXX(dinosaure): it's important to return what we can instead
               to fill [raw] as much as we can. Into details, it's pretty close to the TLS
               stack when [Tls.Engine.state] expects to terminate the handshake as soon as
               possible. In this case, pending payload can serve the end of the handshake
               and ask then to [Tls.Engine.state] to send something and go to the next
               action (according the underlying server logic) when the client side, at
               this step expect to read some bytes. *)

            Lwt.return (Ok (`Input consumed))
          | x :: r ->
            let x = Cstruct.of_bigarray x in
            let len = min (Cstruct.len x) (Cstruct.len raw) in
            Cstruct.blit x 0 raw 0 len ;
            if len = Cstruct.len raw
            then ( Log.debug (fun m -> m "<- Shift %d bytes." (consumed + len))
                 ; Ke.N.shift_exn t.queue (consumed + len)
                 ; Lwt.return (Ok (`Input (consumed + len))) )
            else ( go (consumed + len) (Cstruct.shift raw len) r ) in
        go 0 raw lst

    let send t raw =
      Log.debug (fun m -> m "-> Start to write %d byte(s)." (Cstruct.len raw)) ;
      StackV4.TCPV4.write t.flow raw >|= R.reword_error write_error >>= function
      | Error err ->
        Log.err (fun m -> m "-> Got an error when writing: %a" pp_error err) ;
        Lwt.return (Error err)
      | Ok () ->
        Log.debug (fun m -> m "-> Write %d byte(s)." (Cstruct.len raw)) ;
        Lwt.return (Ok (Cstruct.len raw))

    let close t =
      StackV4.TCPV4.close t.flow >>= fun () -> Lwt.return (Ok ())
  end

  let endpoint
    : endpoint Tuyau_mirage.key
    = Tuyau_mirage.key "tcp-mirage"

  let protocol =
    Tuyau_mirage.register_protocol ~key:endpoint ~protocol:(module Protocol)

  type nonrec configuration = StackV4.t configuration

  type service =
    { stack : StackV4.t
    ; queue : StackV4.TCPV4.flow Queue.t
    ; condition : unit Lwt_condition.t
    ; mutex : Lwt_mutex.t
    ; mutable closed : bool }

  module Service = struct
    type +'a s = 'a Tuyau_mirage.s

    type error = Connection_aborted

    let pp_error : error Fmt.t = fun ppf -> function
      | Connection_aborted -> Fmt.string ppf "Connection aborted"

    type flow = protocol
    type endpoint = configuration
    type t = service

    let make { stack; keepalive; port; } =
      let queue = Queue.create () in
      let condition = Lwt_condition.create () in
      let mutex = Lwt_mutex.create () in
      let listener flow =
        Lwt_mutex.lock mutex >>= fun () ->
        Queue.push flow queue ;
        Lwt_condition.signal condition () ;
        Lwt_mutex.unlock mutex ;
        Lwt.return () in
      StackV4.listen_tcpv4 ?keepalive stack ~port listener ;
      Lwt.return (Ok { stack; queue; condition; mutex; closed= false; })

    let rec accept ({ queue; condition; mutex; closed; _ } as t) =
      Lwt_mutex.lock mutex >>= fun () ->
      let rec await () =
        if Queue.is_empty queue && not closed
        then Lwt_condition.wait condition ~mutex >>= await
        else Lwt.return () in
      await () >>= fun () -> match Queue.pop queue with
      | flow ->
        let queue, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
        Lwt_mutex.unlock mutex ;
        Lwt.return (Ok { flow; queue; })
      | exception Queue.Empty ->
        if closed
        then ( Lwt_mutex.unlock mutex ; Lwt.return (Error Connection_aborted))
        else ( Lwt_mutex.unlock mutex ; accept t )

    let close ({ stack; mutex; _ } as t) =
      Lwt_mutex.with_lock mutex
        (fun () -> StackV4.disconnect stack >>= fun () -> t.closed <- true ; Lwt.return (Ok ()))
  end

  let configuration
    : configuration Tuyau_mirage.key
    = Tuyau_mirage.key "tcp-mirage"

  let service =
    Tuyau_mirage.register_service
      ~key:configuration
      ~service:(module Service)
      ~protocol:protocol
end
