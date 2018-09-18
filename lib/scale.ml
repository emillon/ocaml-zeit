type t =
  { current : int
  ; min : int
  ; max : int }
[@@deriving eq, show, of_yojson]
