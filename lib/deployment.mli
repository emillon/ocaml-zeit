type t =
  { uid : string
  ; name : string
  ; url : string
  ; created : int64
  ; type_ : string
  ; creator : string
  ; instanceCount : unit option
  ; scale : Scale.t option
  ; state : string option }
[@@deriving eq, show]

module Api_responses : sig
  type list_result = t list [@@deriving of_yojson]
end
