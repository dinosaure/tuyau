module Make
    (Scheduler : Tuyau.Sigs.SCHEDULER)
    (Tuyau : Tuyau.S with type input = Cstruct.t
                      and type output = Cstruct.t
                      and type +'a s = 'a Scheduler.t)
  : sig
    type 'flow protocol_with_tls

    val underlying : 'flow protocol_with_tls -> 'flow
    val handshake : 'flow protocol_with_tls -> bool

    val protocol_with_tls :
      key:'edn Tuyau.key ->
      'flow Tuyau.Witness.protocol ->
      ('edn * Tls.Config.client) Tuyau.key * 'flow protocol_with_tls Tuyau.Witness.protocol

    type 'service service_with_tls

    val service_with_tls :
      key:'edn Tuyau.key ->
      ('t * 'flow) Tuyau.Witness.service ->
      'flow protocol_with_tls Tuyau.Witness.protocol ->
      ('edn * Tls.Config.server) Tuyau.key * ('t service_with_tls * 'flow protocol_with_tls) Tuyau.Witness.service
  end
