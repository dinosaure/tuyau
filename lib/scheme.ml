type t = string

let of_string x = Ok x
let of_string_exn x = match of_string x with
  | Ok v -> v
  | Error (`Msg err) -> invalid_arg err
let to_string x = x
