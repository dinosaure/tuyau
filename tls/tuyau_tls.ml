module Ke = Ke.Rke
module Sigs = Tuyau.Sigs

module Make
    (Scheduler : Sigs.SCHEDULER)
    (Tuyau : Tuyau.S with type input = Cstruct.t
                      and type output = Cstruct.t
                      and type +'a s = 'a Scheduler.t)
= struct
  let return x = Scheduler.return x
  let ( >>= ) x f = Scheduler.bind x f
  let ( >>| ) x f = x >>= fun x -> return (f x)
  let ( >>? ) x f = x >>= function
    | Ok x -> f x
    | Error err -> return (Error err)

  let reword_error : ('e0 -> 'e1) -> ('a, 'e0) result  -> ('a, 'e1) result = fun f -> function
    | Ok v -> Ok v
    | Error err -> Error (f err)

  let src = Logs.Src.create "tuyau-tls"
  module Log = (val Logs.src_log src : Logs.LOG)

  type 'flow protocol_with_tls =
    { mutable tls : Tls.Engine.state option
    ; raw : Cstruct.t
    ; flow : 'flow
    ; queue : (char, Bigarray.int8_unsigned_elt) Ke.t }

  let underlying { flow; _ } = flow
  let handshake { tls; _ } = match tls with
    | Some tls -> Tls.Engine.handshake_in_progress tls
    | None -> false

  module Make_protocol
      (Flow : Sigs.PROTOCOL with type input = Tuyau.input
                             and type output = Tuyau.output
                             and type +'a s = 'a Scheduler.t)
  = struct
    type input = Tuyau.input
    type output = Tuyau.output
    type +'a s = 'a Tuyau.s

    type endpoint = Flow.endpoint * Tls.Config.client
    type flow = Flow.flow protocol_with_tls

    type error =
      [ `Msg of string
      | `Flow of Flow.error
      | `Full
      | `TLS of Tls.Engine.failure ]

    let pp_error : error Fmt.t = fun ppf -> function
      | `Msg err -> Fmt.string ppf err
      | `Flow err -> Flow.pp_error ppf err
      | `Full -> Fmt.pf ppf "Internal queue is full"
      | `TLS failure -> Fmt.string ppf (Tls.Engine.string_of_failure failure)

    let flow_error err = `Flow err

    let flow_wr_opt
      : Flow.flow -> Cstruct.t option -> (unit, error) result Tuyau.s
      = fun flow -> function
      | None -> return (Ok ())
      | Some raw ->
        Log.debug (fun m -> m "~> Send %d bytes" (Cstruct.len raw)) ;
        let rec go raw =
          Flow.send flow raw >>| reword_error flow_error >>? fun len ->
          let raw = Cstruct.shift raw len in
          if Cstruct.len raw = 0
          then return (Ok ())
          else go raw in
        go raw

    let blit src src_off dst dst_off len =
      let src = Cstruct.to_bigarray src in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len

    let queue_wr_opt queue = function
      | None -> return (Ok ())
      | Some raw ->
        Log.debug (fun m -> m "Fill the queue with %d byte(s)." (Cstruct.len raw)) ;
        Ke.N.push queue ~blit ~length:Cstruct.len ~off:0 raw ;
        return (Ok ())

    let handle_tls
      : Tls.Engine.state -> (char, Bigarray.int8_unsigned_elt) Ke.t -> Flow.flow -> Cstruct.t -> (Tls.Engine.state option, error) result Scheduler.t
      = fun tls queue flow raw ->
        match Tls.Engine.handle_tls tls raw with
        | `Fail (failure, `Response resp) ->
          Log.debug (fun m -> m "|- TLS state: Fail") ;
          flow_wr_opt flow (Some resp) >>? fun () ->
          return (Error (`TLS failure))
        | `Ok (`Alert _alert, `Response resp, `Data data) ->
          Log.debug (fun m -> m "|- TLS state: Alert") ;
          flow_wr_opt flow resp >>? fun () ->
          queue_wr_opt queue data >>? fun () ->
          return (Ok (Some tls))
        | `Ok (`Eof, `Response resp, `Data data) ->
          Log.debug (fun m -> m "|- TLS state: EOF") ;
          flow_wr_opt flow resp >>? fun () ->
          queue_wr_opt queue data >>? fun () ->
          return (Ok None)
        | `Ok (`Ok tls, `Response resp, `Data data) ->
          (* XXX(dinosaure): it seems that decoding TLS inputs can produce
             something bigger than expected. For example, decoding 4096 bytes
             can produce 4119 byte(s). *)
          Log.debug (fun m -> m "|- TLS state: Ok") ;
          flow_wr_opt flow resp >>? fun () ->
          queue_wr_opt queue data >>? fun () ->
          return (Ok (Some tls))

    let handle_handshake
      : Tls.Engine.state -> (char, Bigarray.int8_unsigned_elt) Ke.t -> Flow.flow -> Cstruct.t -> (Tls.Engine.state option, error) result Scheduler.t
      = fun tls queue flow raw0 ->
        let rec go tls raw1 = match Tls.Engine.can_handle_appdata tls with
          | true ->
            Log.debug (fun m -> m "Start to talk with TLS (handshake is done).") ;
            handle_tls tls queue flow raw1
          | false ->
            assert (Tls.Engine.handshake_in_progress tls = true) ;
            Log.debug (fun m -> m "Process TLS handshake.") ;

            (* XXX(dinosaure): assertion, [Tls.Engine.handle_tls] consumes all
               bytes of [raw1] and [raw1] is physically a subset of [raw0] (or
               is [raw0]). we can re-use [raw0] for [Flow.recv] safely. *)

            match Tls.Engine.handle_tls tls raw1 with
            | `Ok (`Ok tls, `Response resp, `Data data) ->
              Log.debug (fun m -> m "-- TLS state: OK") ;
              flow_wr_opt flow resp >>? fun () ->
              queue_wr_opt queue data >>? fun () ->
              if Tls.Engine.handshake_in_progress tls
              then
                ( Log.debug (fun m -> m "<- Read the TLS flow")
                ; Flow.recv flow raw0 >>| reword_error flow_error >>? function
                | `End_of_input -> return (Ok None)
                | `Input len ->
                  let uid = Hashtbl.hash (Cstruct.to_string (Cstruct.sub raw0 0 len)) in
                  Log.debug (fun m -> m "<~ [%04x] Got %d bytes (handshake in progress: true)." uid len) ;
                  go tls (Cstruct.sub raw0 0 len) )
              else ( Log.debug (fun m -> m "Handshake is done.") ; return (Ok (Some tls)) )
            | `Ok (`Eof, `Response resp, `Data data) ->
              Log.debug (fun m -> m "-- TLS state: EOF") ;
              flow_wr_opt flow resp >>? fun () ->
              queue_wr_opt queue data >>? fun () ->
              return (Ok None)
            | `Fail (failure, `Response resp) ->
              Log.debug (fun m -> m "-- TLS state: Fail") ;
              flow_wr_opt flow (Some resp) >>? fun () ->
              return (Error (`TLS failure))
            | `Ok (`Alert _alert, `Response resp, `Data data) ->
              Log.debug (fun m -> m "-- TLS state: Alert") ;
              flow_wr_opt flow resp >>? fun () ->
              queue_wr_opt queue data >>? fun () ->
              return (Ok (Some tls))
        in go tls raw0

    let flow (edn, config) =
      Flow.flow edn >>| reword_error flow_error >>? fun flow ->
      let raw = Cstruct.create 0x1000 in
      let queue = Ke.create ~capacity:0x1000 Bigarray.Char in
      let tls, buf = Tls.Engine.client config in
      let rec go buf =
        Log.debug (fun m -> m "Start handshake.") ;
        Flow.send flow buf >>| reword_error flow_error >>? fun len ->
        let buf = Cstruct.shift buf len in
        if Cstruct.len buf = 0
        then return (Ok { tls= Some tls; raw; queue; flow; })
        else go buf in
      go buf

    let blit src src_off dst dst_off len =
      let dst = Cstruct.to_bigarray dst in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len

    let rec recv t raw =
      Log.debug (fun m -> m "<~ Start to receive.") ;
      match Ke.N.peek t.queue with
      | [] ->
        Log.debug (fun m -> m "<~ TLS queue is empty.") ;
        ( match t.tls with
          | None ->
            Log.debug (fun m -> m "<~ Connection is close.") ;
            return (Ok `End_of_input)
          | Some tls ->
            Log.debug (fun m -> m "<- Read the TLS flow.") ;
            Flow.recv t.flow t.raw >>| reword_error flow_error >>? function
            | `End_of_input ->
              Log.debug (fun m -> m "<- Connection closed by underlying protocol.")
            ; t.tls <- None
            ; return (Ok `End_of_input)
            | `Input len ->
              let handle =
                if Tls.Engine.handshake_in_progress tls
                then handle_handshake tls t.queue t.flow
                else handle_tls tls t.queue t.flow in
              let uid = Hashtbl.hash (Cstruct.to_string (Cstruct.sub t.raw 0 len)) in
              Log.debug (fun m -> m "<~ [%04x] Got %d bytes (handshake in progress: %b)."
                            uid len (Tls.Engine.handshake_in_progress tls)) ;
              handle (Cstruct.sub t.raw 0 len) >>? fun tls ->
              t.tls <- tls ; recv t raw )
      | _ ->
        let max = Cstruct.len raw in
        let len = min (Ke.length t.queue) max in
        Ke.N.keep_exn t.queue ~blit ~length:Cstruct.len ~off:0 ~len raw ;
        Ke.N.shift_exn t.queue len ;
        return (Ok (`Input len))

    let rec send t raw =
      Log.debug (fun m -> m "~> Start to send.") ;
      match t.tls with
      | None -> return (Ok 0)
      | Some tls when Tls.Engine.can_handle_appdata tls ->
        let raw = [ raw ] in
        ( match Tls.Engine.send_application_data tls raw with
        | Some (tls, resp) ->
          flow_wr_opt t.flow (Some resp) >>? fun () ->
          t.tls <- Some tls ; return (Ok (Cstruct.lenv raw))
        | None ->
          return (Ok (Cstruct.lenv raw)) )
      | Some tls ->
        Flow.recv t.flow t.raw >>| reword_error flow_error >>? function
        | `End_of_input ->
          t.tls <- None ; return (Ok 0)
        | `Input len ->
          let res = handle_handshake tls t.queue t.flow (Cstruct.sub t.raw 0 len) in
          res >>= function
            | Ok tls -> t.tls <- tls ; send t raw
            | Error _ as err ->
              Log.err (fun m -> m "[-] Got an error during handshake.") ;
              return err

    let close t =
      match t.tls with
      | None ->
        Flow.close t.flow >>| reword_error flow_error
      | Some tls ->
        let _tls, resp = Tls.Engine.send_close_notify tls in
        t.tls <- None ;
        Log.debug (fun m -> m "!- Close the connection.") ;
        flow_wr_opt t.flow (Some resp) >>? fun () ->
        Flow.close t.flow >>| reword_error flow_error >>? fun () ->
        return (Ok ())
  end

  let protocol_with_tls
    : type edn flow. key:edn Tuyau.key -> flow Tuyau.Witness.protocol -> (edn * Tls.Config.client) Tuyau.key * flow protocol_with_tls Tuyau.Witness.protocol
    = fun ~key protocol ->
      match Tuyau.impl_of_protocol ~key protocol with
      | Ok (module Flow) ->
        let module M = Make_protocol(Flow) in
        let k = Tuyau.key (Fmt.strf "%s + tls" (Tuyau.name_of_key key)) in
        let p = Tuyau.register_protocol ~key:k ~protocol:(module M) in
        k, p
      | Error _ -> assert false

  type 'service service_with_tls =
    { service : 'service
    ; tls : Tls.Config.server }

  module Make_server
      (Service : Sigs.SERVICE with type +'a s = 'a Scheduler.t)
  = struct
    type +'a s = 'a Tuyau.s

    type endpoint = Service.endpoint * Tls.Config.server
    type flow = Service.flow protocol_with_tls

    type error = [ `Service of Service.error ]

    let pp_error : error Fmt.t = fun ppf -> function
      | `Service err -> Service.pp_error ppf err

    let service_error err = `Service err

    type t = Service.t service_with_tls

    let make (edn, tls) =
      Service.make edn >>| reword_error service_error >>? fun service ->
      Log.info (fun m -> m "Start a TLS service.") ;
      return (Ok { service; tls; })

    let accept { service; tls; } =
      Service.accept service >>| reword_error service_error >>? fun flow ->
      let tls = Tls.Engine.server tls in
      let raw = Cstruct.create 0x1000 in
      let queue = Ke.create ~capacity:0x1000 Bigarray.Char in
      Log.info (fun m -> m "A TLS flow is coming.") ;
      return (Ok { tls= Some tls; raw; queue; flow; })

    let close { service; _ } =
      Service.close service >>| reword_error service_error
  end

  let service_with_tls
    : type edn t flow. key:edn Tuyau.key -> (t * flow) Tuyau.Witness.service -> flow protocol_with_tls Tuyau.Witness.protocol ->
      (edn * Tls.Config.server) Tuyau.key * (t service_with_tls * flow protocol_with_tls) Tuyau.Witness.service
    = fun ~key service protocol ->
      match Tuyau.impl_of_service ~key service with
      | Ok (module Service) ->
        let module M = Make_server(Service) in
        let k = Tuyau.key (Fmt.strf "%s + tls" (Tuyau.name_of_key key)) in
        let s = Tuyau.register_service ~key:k ~service:(module M) ~protocol in
        k, s
      | _ -> assert false
end
