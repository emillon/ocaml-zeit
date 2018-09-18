open OUnit2

let cases = ( >::: )

module Cohttp_mock = struct
  let call mock headers uri = Mock.call mock (headers, uri)

  let configure mock ~status ~body =
    Mock.configure mock
      (Mock.return (Lwt.return (Cohttp.Response.make ~status (), body)))


  let equal_cohttp_header a b =
    [%eq: (string * string) list] (Cohttp.Header.to_list a)
      (Cohttp.Header.to_list b)


  let pp_cohttp_header fmt h =
    Format.pp_print_string fmt (Cohttp.Header.to_string h)


  let pp_uri fmt u = Format.pp_print_string fmt (Uri.to_string u)

  let assert_called_once_with ~ctxt expected_args mock =
    Mock_ounit.assert_called_once_with ~ctxt ~cmp:[%eq: cohttp_header * Uri.t]
      ~printer:[%show: cohttp_header * uri] expected_args mock
end

let auth_headers token =
  Cohttp.Header.of_list [("Authorization", "Bearer " ^ token)]


let case_lwt s l = s >:: fun ctxt -> Lwt_main.run (l ctxt)

let test_list_deployments =
  let token = "TOKEN" in
  let make_client () =
    let mock = Mock.make ~name:"cohttp_get" in
    let cohttp_get = Cohttp_mock.call mock in
    let client = Zeit.Client.make ~cohttp_get ~token () in
    (client, mock)
  in
  let test ?(status = `OK) ?(body = "") ~expected () ctxt =
    let open Zeit.Let.Lwt in
    let client, mock = make_client () in
    let body = Cohttp_lwt.Body.of_string body in
    Cohttp_mock.configure mock ~status ~body;
    let%map got = Zeit.Client.list_deployments client in
    let expected_uri = "https://api.zeit.co/v2/now/deployments" in
    let expected_args = (auth_headers token, Uri.of_string expected_uri) in
    assert_equal ~ctxt
      ~cmp:[%eq: (Zeit.Deployment.t list, Zeit.Error.t) result]
      ~printer:[%show: (Zeit.Deployment.t list, Zeit.Error.t) result] expected
      got;
    Cohttp_mock.assert_called_once_with ~ctxt expected_args mock
  in
  cases "list_deployments"
    [ case_lwt "HTTP error"
        (test ~status:`Unauthorized ~expected:(Error Http_error) ())
    ; case_lwt "JSON error" (test ~expected:(Error Json_error) ())
    ; case_lwt "Deserialization error"
        (test ~body:"{}" ~expected:(Error Deserialization_error) ())
    ; case_lwt "OK" (test ~body:"{\"deployments\":[]}" ~expected:(Ok []) ())
    ]


let suite = cases "client" [test_list_deployments]
