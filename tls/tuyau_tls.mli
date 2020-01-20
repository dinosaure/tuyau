module Make
    (Scheduler : Tuyau.Sigs.SCHEDULER)
    (Tuyau : Tuyau.S with type input = Bytes.t
                      and type output = String.t
                      and type +'a s = 'a Scheduler.t)
  : sig
    type 'flow flow_with_tls

    val flow_with_tls :
      key:'edn Tuyau.key ->
      'flow Tuyau.Witness.protocol ->
      ('edn * [ `As_server of Tls.Config.server | `As_client of Tls.Config.client ]) Tuyau.key * 'flow flow_with_tls Tuyau.Witness.protocol

    type 'service service_with_tls

    val service_with_tls :
      key:'edn Tuyau.key ->
      ('t * 'flow) Tuyau.Witness.service ->
      'flow flow_with_tls Tuyau.Witness.protocol ->
      ('edn * Tls.Config.server) Tuyau.key * ('t service_with_tls * 'flow flow_with_tls) Tuyau.Witness.service
  end
