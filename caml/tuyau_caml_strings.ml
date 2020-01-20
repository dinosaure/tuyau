type flow =
  { mutable input : string list
  ; output : string Queue.t }

module Strings_protocol = struct
  type input = Bytes.t
  type output = String.t
  type +'a s = 'a Tuyau_caml.s

  type error = |

  let pp_error : error Fmt.t = fun _ppf -> function _ -> .

  type endpoint = string list
  type nonrec flow = flow =
    { mutable input : string list
    ; output : string Queue.t }

  let flow input = Ok { input; output= Queue.create (); }

  let recv flow buf =
    let max = Bytes.length buf in
    let rec go dst_off contents =
      if dst_off = Bytes.length buf
      then ( flow.input <- contents ; Ok (`Input buf) )
      else match contents with
        | [] ->
          let buf = Bytes.sub buf 0 dst_off in
          flow.input <- contents ; Ok (`Input buf)
        | x :: r ->
          let len = min (String.length x) (max - dst_off) in
          Bytes.blit_string x 0 buf dst_off len ;
          if len = String.length x
          then go (dst_off + len) r
          else ( flow.input <- String.sub x len (String.length x - len) :: r
               ; Ok (`Input buf) ) in
    match flow.input with
    | [] -> Ok `End_of_input
    | contents -> go 0 contents

  let send flow str =
    Queue.add str flow.output ;
    Ok (String.length str)

  let close flow = flow.input <- [] ; Ok ()
end

let strings : string list Tuyau_caml.key = Tuyau_caml.key ~name:"strings"
let strings_protocol = Tuyau_caml.register_protocol ~key:strings ~protocol:(module Strings_protocol)
