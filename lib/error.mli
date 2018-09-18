type t =
  | Http_error
  | Json_error of string

val to_string : t -> string
