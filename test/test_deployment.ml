open OUnit2

let cases = ( >::: )

let case = ( >:: )

let test_list_result_of_yojson =
  let test json expected ctxt =
    let got = Zeit.Deployment.Api_responses.list_result_of_yojson json in
    assert_equal ~ctxt ~cmp:[%eq: (Zeit.Deployment.t list, string) result]
      ~printer:[%show: (Zeit.Deployment.t list, string) result] expected got
  in
  let name = "NAME" in
  let creator = "CREATOR" in
  let uid1 = "U1" in
  let uid2 = "U2" in
  let url1 = "UU1.XX.YY" in
  let url2 = "UU2.XX.YY" in
  let deployment1 =
    { Zeit.Deployment.uid = uid1
    ; name
    ; url = url1
    ; created = 1L
    ; type_ = "STATIC"
    ; creator
    ; instanceCount = None
    ; scale = None
    ; state = None }
  in
  let deployment2 =
    { Zeit.Deployment.uid = uid2
    ; name
    ; url = url2
    ; created = 2L
    ; state = Some "BUILDING"
    ; instanceCount = None
    ; type_ = "DOCKER"
    ; creator
    ; scale = Some {current = 0; min = 0; max = 10} }
  in
  let json_ok =
    `Assoc
      [ ( "deployments"
        , `List
            [ `Assoc
                [ ("uid", `String uid1)
                ; ("name", `String name)
                ; ("url", `String url1)
                ; ("created", `Int 1)
                ; ("type", `String "STATIC")
                ; ("creator", `Assoc [("uid", `String creator)])
                ; ("instanceCount", `Null)
                ; ("scale", `Null) ]
            ; `Assoc
                [ ("uid", `String uid2)
                ; ("name", `String name)
                ; ("url", `String url2)
                ; ("created", `Int 2)
                ; ("state", `String "BUILDING")
                ; ("type", `String "DOCKER")
                ; ("creator", `Assoc [("uid", `String creator)])
                ; ( "scale"
                  , `Assoc
                      [("current", `Int 0); ("min", `Int 0); ("max", `Int 10)]
                  ) ] ] ) ]
  in
  cases "list_deployment_response_of_yojson"
    [case "ok" (test json_ok (Ok [deployment1; deployment2]))]


let suite =
  cases "deployment" [cases "api_responses" [test_list_result_of_yojson]]
