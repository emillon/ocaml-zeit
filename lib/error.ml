type t =
  | Http_error
  | Json_error
  | Deserialization_error
[@@deriving eq, show]

let to_string = function
  | Http_error ->
      "HTTP error"
  | Json_error ->
      "JSON error"
  | Deserialization_error ->
      "Deserialization error"
