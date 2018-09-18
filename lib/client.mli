type t =
  { token : string
  ; host : string }

val list_deployments : t -> (Deployment.t list, Error.t) result Lwt.t
