type t =
  { token : string
  ; host : string
  ; cohttp_get :
         Cohttp.Header.t
      -> Uri.t
      -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t }

let default_cohttp_get headers uri = Cohttp_lwt_unix.Client.get ~headers uri

let make ?(cohttp_get = default_cohttp_get) ~token () =
  {token; host = "api.zeit.co"; cohttp_get}


let uri client route =
  Uri.make ~scheme:"https" ~host:client.host ~path:(Route.path route) ()


let request ~client route =
  let open Let.Lwt in
  let headers =
    Cohttp.Header.init_with "Authorization" ("Bearer " ^ client.token)
  in
  let%map resp, body = client.cohttp_get headers (uri client route) in
  let code =
    Cohttp.Code.code_of_status (Cohttp_lwt_unix.Response.status resp)
  in
  if Cohttp.Code.is_success code then Ok body else Error Error.Http_error


let parse_and_convert s of_yojson =
  match Yojson.Safe.from_string s with
  | exception Yojson.Json_error _ ->
      Error Error.Json_error
  | json -> (
    match of_yojson json with
    | Ok _ as r ->
        r
    | Error _ ->
        Error Error.Deserialization_error )


let list_deployments client =
  let open Let.Lwt in
  let%bind req_rv = request ~client Deployment_list in
  match req_rv with
  | Error e ->
      Lwt.return_error e
  | Ok body ->
      let%map body = Cohttp_lwt.Body.to_string body in
      parse_and_convert body Deployment.Api_responses.list_result_of_yojson
