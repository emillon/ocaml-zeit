type t =
  | Http_error
  | Json_error
  | Deserialization_error
[@@deriving eq, show]

val to_string : t -> string
