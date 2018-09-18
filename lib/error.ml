type t =
  | Http_error
  | Json_error of string

let to_string = function
  | Http_error ->
      "HTTP error"
  | Json_error e ->
      e
