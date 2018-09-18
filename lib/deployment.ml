module Creator = struct
  type t = string [@@deriving eq, show]

  let _ = show

  type repr = {uid : string} [@@deriving of_yojson]

  let of_yojson json =
    let open Let.Json in
    let%map {uid} = repr_of_yojson json in
    uid
end

type t =
  { uid : string
  ; name : string
  ; url : string
  ; created : int64
  ; type_ : string [@key "type"]
  ; creator : Creator.t
  ; instanceCount : unit option [@default None]
  ; scale : Scale.t option
  ; state : string option [@default None] }
[@@deriving eq, show, of_yojson]

module Api_responses = struct
  type list_result = t list

  type list_result_repr = {deployments : t list} [@@deriving of_yojson]

  let list_result_of_yojson json =
    let open Let.Json in
    let%map {deployments} = list_result_repr_of_yojson json in
    deployments


  type create_result =
    { deploymentId : string
    ; url : string
    ; readyState : string }
  [@@deriving eq, show, of_yojson]
end
