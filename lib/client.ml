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
  Lwt.return
  @@
  match Yojson.Safe.from_string s with
  | exception Yojson.Json_error _ ->
      Error Error.Json_error
  | json -> (
    match of_yojson json with
    | Ok _ as r ->
        r
    | Error _ ->
        Error Error.Deserialization_error )


let body_to_string body =
  let open Let.Lwt in
  let%map s = Cohttp_lwt.Body.to_string body in
  Ok s


let list_deployments client =
  let open Let.Lwt_result in
  let%bind cohttp_body = request ~client `GET Deployment_list "" in
  let%bind body = body_to_string cohttp_body in
  parse_and_convert body Deployment.Api_responses.list_result_of_yojson


let post_file client s =
  let open Let.Lwt_result in
  let sha1 = Digestif.SHA1.to_hex @@ Digestif.SHA1.digest_string s in
  let file_size = string_of_int @@ String.length s in
  let extra_headers =
    [ ("Content-Length", file_size)
    ; ("Content-Type", "application/octet-stream")
    ; ("x-now-digest", sha1)
    ; ("x-now-size", file_size) ]
  in
  let%map _ : Cohttp_lwt.Body.t =
    request ~client ~extra_headers `POST Post_file s
  in
  sha1


type file = string * string * int

type file_repr =
  { file : string
  ; sha : string
  ; size : int }
[@@deriving to_yojson]

let file_to_yojson (file, sha, size) = file_repr_to_yojson {file; sha; size}

type create_deployment_body =
  { name : string
  ; public : bool
  ; deploymentType : string
  ; files : file list }
[@@deriving to_yojson]

let create_deployment client ~name ~files =
  let open Let.Lwt_result in
  let body = {name; public = true; deploymentType = "STATIC"; files} in
  let encoded_body =
    Yojson.Safe.to_string @@ create_deployment_body_to_yojson body
  in
  let%bind (cohttp_body : Cohttp_lwt.Body.t) =
    request ~client ~extra_headers:[] `POST Create_deployment encoded_body
  in
  let%bind body = body_to_string cohttp_body in
  parse_and_convert body Deployment.Api_responses.create_result_of_yojson
