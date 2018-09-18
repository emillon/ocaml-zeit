type t =
  { token : string
  ; host : string }

let uri client route =
  Uri.make ~scheme:"https" ~host:client.host ~path:(Route.path route) ()


let request ~client route =
  let open Let.Lwt in
  let headers =
    Cohttp.Header.init_with "Authorization" ("Bearer " ^ client.token)
  in
  let%map resp, body =
    Cohttp_lwt_unix.Client.get ~headers (uri client route)
  in
  let code =
    Cohttp.Code.code_of_status (Cohttp_lwt_unix.Response.status resp)
  in
  if Cohttp.Code.is_success code then Ok body else Error Error.Http_error


let lwt_unwrap = function
  | Error e ->
      Lwt.fail_with (Error.to_string e)
  | Ok r ->
      Lwt.return r


let rewrite_json_error = function
  | Ok _ as r ->
      r
  | Error e ->
      Error (Error.Json_error e)


let list_deployments client =
  let open Let.Lwt in
  let%bind req_rv = request ~client Deployment_list in
  let%bind body = lwt_unwrap req_rv in
  let%map body = Cohttp_lwt.Body.to_string body in
  body
  |> Yojson.Safe.from_string
  |> Deployment.Api_responses.list_result_of_yojson
  |> rewrite_json_error
