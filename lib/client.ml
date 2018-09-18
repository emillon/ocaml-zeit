type t =
  { token : string
  ; host : string
  ; cohttp_call :
         Cohttp.Code.meth
      -> Cohttp.Header.t
      -> Uri.t
      -> body:string
      -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t }

let default_cohttp_call meth headers uri ~body =
  let body = Cohttp_lwt.Body.of_string body in
  Cohttp_lwt_unix.Client.call meth ~headers ~body uri


let make ?(cohttp_call = default_cohttp_call) ~token () =
  {token; host = "api.zeit.co"; cohttp_call}


let uri client route =
  Uri.make ~scheme:"https" ~host:client.host ~path:(Route.path route) ()


let request ~client ?(extra_headers = []) meth route body =
  let open Let.Lwt in
  let base_header =
    Cohttp.Header.init_with "Authorization" ("Bearer " ^ client.token)
  in
  let headers = Cohttp.Header.add_list base_header extra_headers in
  let%map resp, body =
    client.cohttp_call meth headers (uri client route) ~body
  in
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
  let%bind req_rv = request ~client `GET Deployment_list "" in
  match req_rv with
  | Error e ->
      Lwt.return_error e
  | Ok body ->
      let%map body = Cohttp_lwt.Body.to_string body in
      parse_and_convert body Deployment.Api_responses.list_result_of_yojson


let post_file client s =
  let open Let.Lwt in
  let sha1 = Digestif.SHA1.to_hex @@ Digestif.SHA1.digest_string s in
  let file_size = string_of_int @@ String.length s in
  let extra_headers =
    [ ("Content-Length", file_size)
    ; ("Content-Type", "application/octet-stream")
    ; ("x-now-digest", sha1)
    ; ("x-now-size", file_size) ]
  in
  match%map request ~client ~extra_headers `POST Post_file s with
  | Ok (_ : Cohttp_lwt.Body.t) ->
      Ok sha1
  | Error _ as e ->
      e
