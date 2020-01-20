module Ke = Ke.Rke.Weighted
module Sigs = Tuyau.Sigs

module Make
    (Scheduler : Sigs.SCHEDULER)
    (Tuyau : Tuyau.S with type input = Bytes.t
                      and type output = String.t
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

  type 'flow flow_with_tls =
    { mutable tls : Tls.Engine.state option
    ; buf : Bytes.t
    ; flw : 'flow
    ; rbf : (char, Bigarray.int8_unsigned_elt) Ke.t }

  module Make_protocol
      (Flow : Sigs.F with type input = Tuyau.input
                      and type output = Tuyau.output
                      and type +'a s = 'a Scheduler.t)
  = struct
    type input = Tuyau.input
    type output = Tuyau.output
    type +'a s = 'a Tuyau.s

    type endpoint = Flow.endpoint * [ `As_server of Tls.Config.server
                                    | `As_client of Tls.Config.client ]
    type flow = Flow.flow flow_with_tls

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
        let raw = Cstruct.to_string raw in

        let rec go raw : (unit, error) result Scheduler.t =
          Flow.send flow raw >>| reword_error flow_error >>? fun len ->
          let raw = String.(sub raw len (length raw - len)) in
          if String.length raw = 0
          then return (Ok ())
          else go raw in
        go raw

    let blit src src_off dst dst_off len =
      let src = Cstruct.to_bigarray src in
      Bigstringaf.blit src ~src_off dst ~dst_off ~len

    let queue_wr_opt queue = function
      | None -> return (Ok ())
      | Some raw ->
        match Ke.N.push queue ~blit ~length:Cstruct.len ~off:0 raw with
        | Some _ -> return (Ok ())
        | None -> return (Error `Full)

    let handle_tls
      : Tls.Engine.state -> (char, Bigarray.int8_unsigned_elt) Ke.t -> Flow.flow -> Bytes.t -> (Tls.Engine.state option, error) result Scheduler.t
      = fun tls queue flow raw ->
        let raw = Cstruct.of_bytes raw in

        match Tls.Engine.handle_tls tls raw with
        | `Fail (failure, `Response resp) ->
          flow_wr_opt flow (Some resp) >>? fun () ->
          return (Error (`TLS failure))
        | `Ok (`Alert _alert, `Response resp, `Data data) ->
          flow_wr_opt flow resp >>? fun () ->
          queue_wr_opt queue data >>? fun () ->
          return (Ok (Some tls))
        | `Ok (`Eof, `Response resp, `Data data) ->
          flow_wr_opt flow resp >>? fun () ->
          queue_wr_opt queue data >>? fun () ->
          return (Ok None)
        | `Ok (`Ok tls, `Response resp, `Data data) ->
          flow_wr_opt flow resp >>? fun () ->
          queue_wr_opt queue data >>? fun () ->
          return (Ok (Some tls))

    let rec handle_handshake
      : Tls.Engine.state -> (char, Bigarray.int8_unsigned_elt) Ke.t -> Flow.flow -> Bytes.t -> (Tls.Engine.state option, error) result Scheduler.t
      = fun tls queue flow raw ->
        Fmt.epr ">> handle handshake.\n%!" ;
        match Tls.Engine.can_handle_appdata tls with
        | true ->
          Fmt.epr ">> ready to talk!\n%!" ;
          handle_tls tls queue flow raw
        | false ->
          assert (Tls.Engine.handshake_in_progress tls = true) ;
          let raw = Cstruct.of_bytes raw in

          match Tls.Engine.handle_tls tls raw with
          | `Ok (`Ok tls, `Response resp, `Data data) ->
            Fmt.epr "?> handshake (%b).\n%!" (Tls.Engine.handshake_in_progress tls) ;
            flow_wr_opt flow resp >>? fun () ->
            queue_wr_opt queue data >>? fun () ->
            if Tls.Engine.handshake_in_progress tls
            then
              let buf = Bytes.create 0x1000 in
              Flow.recv flow buf >>| reword_error flow_error >>? function
              | `End_of_input ->
                Flow.close flow >>| reword_error flow_error >>? fun () ->
                return (Ok None)
              | `Input buf ->
                handle_handshake tls queue flow buf
            else return (Ok (Some tls))
          | `Ok (`Eof, `Response resp, `Data data) ->
            flow_wr_opt flow resp >>? fun () ->
            queue_wr_opt queue data >>? fun () ->
            Flow.close flow >>| reword_error flow_error >>? fun () ->
            return (Ok None)
          | `Fail (failure, `Response resp) ->
            flow_wr_opt flow (Some resp) >>? fun () ->
            Flow.close flow >>| reword_error flow_error >>? fun () ->
            return (Error (`TLS failure))
          | `Ok (`Alert _alert, `Response resp, `Data data) ->
            Fmt.epr ">> alert.\n%!" ;
            flow_wr_opt flow resp >>? fun () ->
            queue_wr_opt queue data >>? fun () ->
            return (Ok (Some tls))

    let flow (edn, config) =
      Flow.flow edn >>| reword_error flow_error >>? fun flw ->
      let buf = Bytes.create 0x1000 in
      let rbf, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
      match config with
      | `As_server config ->
        let tls = Tls.Engine.server config in
        ( Flow.recv flw buf >>| reword_error flow_error >>? function
            | `End_of_input ->
              Flow.close flw >>| reword_error flow_error >>? fun () ->
              return (Ok { tls= None; buf; rbf; flw; })
            | `Input buf ->
              handle_handshake tls rbf flw buf >>? fun tls ->
              return (Ok { tls; buf; rbf; flw; }) )
      | `As_client config ->
        let tls, raw = Tls.Engine.client config in
        let rec go raw =
          Flow.send flw (Cstruct.to_string raw) >>| reword_error flow_error >>? fun len ->
          let raw = Cstruct.shift raw len in
          if Cstruct.len raw = 0
          then ( Fmt.epr ">> client initialized.\n%!"
               ; return (Ok { tls= Some tls; buf; rbf; flw; }) )
          else go raw in
        go raw

    let rec recv t raw =
      Fmt.epr "<~ recv.\n%!" ;
      match Ke.N.peek t.rbf with
      | [] ->
        Fmt.epr "<< no input.\n%!" ;
        ( match t.tls with
          | None ->
            Flow.close t.flw >>| reword_error flow_error >>? fun () ->
            return (Ok `End_of_input)
          | Some tls ->
            Flow.recv t.flw t.buf >>| reword_error flow_error >>? function
            | `End_of_input ->
              Flow.close t.flw >>| reword_error flow_error >>? fun () ->
              t.tls <- None ; return (Ok `End_of_input)
            | `Input buf ->
              let handle =
                if Tls.Engine.handshake_in_progress tls
                then handle_handshake tls t.rbf t.flw
                else handle_tls tls t.rbf t.flw in
              handle buf >>? fun tls ->
              t.tls <- tls ; recv t raw )
      | lst ->
        Fmt.epr "<< %d byte(s) available.\n%!" (List.fold_left (fun a x -> Bigstringaf.length x + a) 0 lst) ;
        let max = Bytes.length raw in
        let rec go dst_off = function
          | [] ->
            Ke.N.shift_exn t.rbf dst_off ;
            return (Ok (`Input (Bytes.sub raw 0 dst_off)))
          | src :: rest ->
            let len = min (Bigstringaf.length src) (max - dst_off) in
            Bigstringaf.blit_to_bytes src ~src_off:0 raw ~dst_off:0 ~len ;
            if dst_off + len = max
            then ( Ke.N.shift_exn t.rbf max ; return (Ok (`Input raw)) )
            else go (dst_off + len) rest in
        go 0 lst

    let rec send t raw =
      Fmt.epr "~> send.\n%!" ;
      match t.tls with
      | None -> return (Ok 0)
      | Some tls when Tls.Engine.can_handle_appdata tls ->
        let raw = [ Cstruct.of_string raw ] in
        ( match Tls.Engine.send_application_data tls raw with
        | Some (tls, resp) ->
          flow_wr_opt t.flw (Some resp) >>? fun () ->
          t.tls <- Some tls ; return (Ok (Cstruct.lenv raw))
        | None ->
          return (Ok (Cstruct.lenv raw)) )
      | Some tls ->
        Flow.recv t.flw t.buf >>| reword_error flow_error >>? function
        | `End_of_input ->
          t.tls <- None ; return (Ok 0)
        | `Input buf ->
          handle_handshake tls t.rbf t.flw buf >>? fun tls ->
          Fmt.epr ">> handshake done.\n%!" ;
          t.tls <- tls ; send t raw

    let close t =
      match t.tls with
      | None -> return (Ok ())
      | Some tls ->
        let _tls, resp = Tls.Engine.send_close_notify tls in
        t.tls <- None ;
        flow_wr_opt t.flw (Some resp) >>? fun () ->
        return (Ok ())
  end

  let flow_with_tls
    : type edn flow. key:edn Tuyau.key -> flow Tuyau.Witness.protocol -> (edn * [ `As_server of Tls.Config.server | `As_client of Tls.Config.client ]) Tuyau.key * flow flow_with_tls Tuyau.Witness.protocol
    = fun ~key protocol ->
      match Tuyau.protocol ~key protocol with
      | Ok (module Flow) ->
        let module M = Make_protocol(Flow) in
        let k = Tuyau.key ~name:"with_tls" in
        let p = Tuyau.register_protocol ~key:k ~protocol:(module M) in
        k, p
      | Error _ -> assert false

  type 'service service_with_tls =
    { svc : 'service
    ; tls : Tls.Config.server }

  module Make_server
      (Service : Sigs.S with type +'a s = 'a Scheduler.t)
  = struct
    type +'a s = 'a Tuyau.s

    type endpoint = Service.endpoint * Tls.Config.server
    type flow = Service.flow flow_with_tls

    type error = [ `Service of Service.error ]

    let pp_error : error Fmt.t = fun ppf -> function
      | `Service err -> Service.pp_error ppf err

    let service_error err = `Service err

    type t = Service.t service_with_tls

    let make (edn, tls) =
      Service.make edn >>| reword_error service_error >>? fun svc -> return (Ok { svc; tls; })

    let accept { svc; tls; } =
      Service.accept svc >>| reword_error service_error >>? fun flw ->
      let tls = Tls.Engine.server tls in
      let buf = Bytes.create 0x1000 in
      let rbf, _ = Ke.create ~capacity:0x1000 Bigarray.Char in
      return (Ok { tls= Some tls; buf; rbf; flw; })
  end

  let service_with_tls
    : type edn t flow. key:edn Tuyau.key -> (t * flow) Tuyau.Witness.service -> flow flow_with_tls Tuyau.Witness.protocol ->
      (edn * Tls.Config.server) Tuyau.key * (t service_with_tls * flow flow_with_tls) Tuyau.Witness.service
    = fun ~key service protocol ->
      match Tuyau.server ~key service with
      | Ok (module Service) ->
        let module M = Make_server(Service) in
        let k = Tuyau.key ~name:"with_tls" in
        let s = Tuyau.register_service ~key:k ~service:(module M) ~protocol in
        k, s
      | _ -> assert false
end
