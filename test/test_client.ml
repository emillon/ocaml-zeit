open OUnit2

let cases = ( >::: )

module Cohttp_mock = struct
  let call mock meth headers uri ~body =
    Mock.call mock (meth, headers, uri, body)


  let configure mock ~status ~body =
    Mock.configure mock
      (Mock.return (Lwt.return (Cohttp.Response.make ~status (), body)))


  let equal_meth a b =
    String.equal
      (Cohttp.Code.string_of_method a)
      (Cohttp.Code.string_of_method b)


  let pp_meth fmt m =
    Format.pp_print_string fmt (Cohttp.Code.string_of_method m)


  let equal_cohttp_header a b =
    [%eq: (string * string) list] (Cohttp.Header.to_list a)
      (Cohttp.Header.to_list b)


  let pp_cohttp_header fmt h =
    Format.pp_print_string fmt (Cohttp.Header.to_string h)


  let pp_uri fmt u = Format.pp_print_string fmt (Uri.to_string u)

  let assert_called_once_with ~ctxt ~expected_meth ~expected_headers
      ~expected_uri ~expected_body mock =
    let expected_args =
      (expected_meth, expected_headers, expected_uri, expected_body)
    in
    Mock_ounit.assert_called_once_with ~ctxt
      ~cmp:[%eq: meth * cohttp_header * Uri.t * string]
      ~printer:[%show: meth * cohttp_header * uri * string] expected_args mock
end

let case_lwt s l = s >:: fun ctxt -> Lwt_main.run (l ctxt)

let with_client k ~ctxt ~status ~body ~expected_meth ~expected_extra_headers
    ~expected_uri ~expected_body =
  let token = "TOKEN" in
  let expected_headers =
    Cohttp.Header.of_list
      (("Authorization", "Bearer " ^ token) :: expected_extra_headers)
  in
  let mock = Mock.make ~name:"cohttp_call" in
  Cohttp_mock.configure mock ~status ~body;
  let cohttp_call = Cohttp_mock.call mock in
  let client = Zeit.Client.make ~cohttp_call ~token () in
  let result = k client in
  Cohttp_mock.assert_called_once_with ~ctxt ~expected_meth ~expected_headers
    ~expected_uri:(Uri.of_string expected_uri)
    ~expected_body mock;
  result


let test_list_deployments =
  let test ?(status = `OK) ?(body = "") ~expected () ctxt =
    let body = Cohttp_lwt.Body.of_string body in
    with_client ~ctxt ~status ~body ~expected_meth:`GET
      ~expected_extra_headers:[] ~expected_body:""
      ~expected_uri:"https://api.zeit.co/v2/now/deployments" (fun client ->
        let open Zeit.Let.Lwt in
        let%map got = Zeit.Client.list_deployments client in
        assert_equal ~ctxt
          ~cmp:[%eq: (Zeit.Deployment.t list, Zeit.Error.t) result]
          ~printer:[%show: (Zeit.Deployment.t list, Zeit.Error.t) result]
          expected got )
  in
  cases "list_deployments"
    [ case_lwt "HTTP error"
        (test ~status:`Unauthorized ~expected:(Error Http_error) ())
    ; case_lwt "JSON error" (test ~expected:(Error Json_error) ())
    ; case_lwt "Deserialization error"
        (test ~body:"{}" ~expected:(Error Deserialization_error) ())
    ; case_lwt "OK" (test ~body:"{\"deployments\":[]}" ~expected:(Ok []) ())
    ]


let test_post_file =
  let test ~contents ~expected_size ~expected_sha1 ~expected ctxt =
    let expected_extra_headers =
      [ ("Content-Type", "application/octet-stream")
      ; ("Content-Length", expected_size)
      ; ("x-now-digest", expected_sha1)
      ; ("x-now-size", expected_size) ]
    in
    let body = Cohttp_lwt.Body.empty in
    with_client ~ctxt ~body ~expected_meth:`POST ~expected_extra_headers
      ~expected_uri:"https://api.zeit.co/v2/now/files" ~expected_body:contents
      (fun client ->
        let open Zeit.Let.Lwt in
        let%map got = Zeit.Client.post_file client contents in
        assert_equal ~ctxt ~cmp:[%eq: (string, Zeit.Error.t) result]
          ~printer:[%show: (string, Zeit.Error.t) result] expected got )
  in
  let contents = "hello" in
  let contents_sha1 = "aaf4c61ddcc5e8a2dabede0f3b482cd9aea9434d" in
  let expected_size = "5" in
  let expected_sha1 = contents_sha1 in
  cases "post_file"
    [ case_lwt "OK"
        (test ~contents ~status:`OK ~expected_size ~expected_sha1
           ~expected:(Ok contents_sha1))
    ; case_lwt "HTTP error"
        (test ~contents ~status:`Unauthorized ~expected_size ~expected_sha1
           ~expected:(Error Http_error)) ]


(*
public	Boolean	Yes	A boolean indicating if the deployment is public. For every deployment done under the OSS plan, this needs to be set to true.
name	String	Yes	A string with the project name used in the deployment URL.
deploymentType	Enum	Yes	A string indicating the type of deployment, it could be NPM, DOCKER or STATIC.
files	List	Yes	A list of maps with the files you want in the deploy.
*)

let test_create_deployment =
  let test ~name ~files ~body ~expected ~expected_body_json ctxt =
    let expected_body = Yojson.Safe.to_string expected_body_json in
    with_client ~ctxt ~status:`OK ~body
      (fun client ->
        let open Zeit.Let.Lwt in
        let%map got = Zeit.Client.create_deployment client ~name ~files in
        assert_equal ~ctxt
          ~cmp:
            [%eq:
              ( Zeit.Deployment.Api_responses.create_result
              , Zeit.Error.t )
              result]
          ~printer:
            [%show:
              ( Zeit.Deployment.Api_responses.create_result
              , Zeit.Error.t )
              result] expected got )
      ~expected_meth:`POST ~expected_extra_headers:[]
      ~expected_uri:"https://api.zeit.co/v3/now/deployments" ~expected_body
  in
  let name = "my-instant-deployment" in
  let file_name = "index.html" in
  let file_sha = "9d8b952309b28f468919f4a585e18b63a14457f2" in
  let file_size = 161 in
  let file = (file_name, file_sha, file_size) in
  let deploymentId = "ID" in
  let url = "URL" in
  let readyState = "READYSTATE" in
  let create_result =
    {Zeit.Deployment.Api_responses.deploymentId; url; readyState}
  in
  let create_result_body =
    Cohttp_lwt.Body.of_string
    @@ Yojson.Safe.to_string
    @@ `Assoc
         [ ("deploymentId", `String deploymentId)
         ; ("url", `String url)
         ; ("readyState", `String readyState) ]
  in
  cases "create_deployment"
    [ case_lwt "OK"
        (test ~name ~files:[file] ~body:create_result_body
           ~expected:(Ok create_result)
           ~expected_body_json:
             (`Assoc
               [ ("name", `String name)
               ; ("public", `Bool true)
               ; ("deploymentType", `String "STATIC")
               ; ( "files"
                 , `List
                     [ `Assoc
                         [ ("file", `String file_name)
                         ; ("sha", `String file_sha)
                         ; ("size", `Int file_size) ] ] ) ])) ]


let suite =
  cases "client"
    [test_list_deployments; test_post_file; test_create_deployment]
