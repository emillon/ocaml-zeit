type t =
  | Deployment_list
  | Post_file
  | Create_deployment

let path = function
  | Deployment_list ->
      "/v2/now/deployments"
  | Post_file ->
      "/v2/now/files"
  | Create_deployment ->
      "/v3/now/deployments"
