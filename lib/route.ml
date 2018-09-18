type t =
  | Deployment_list
  | Post_file

let path = function
  | Deployment_list ->
      "/v2/now/deployments"
  | Post_file ->
      "/v2/now/files"
