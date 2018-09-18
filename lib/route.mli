type t =
  | Deployment_list
  | Post_file
  | Create_deployment

val path : t -> string
