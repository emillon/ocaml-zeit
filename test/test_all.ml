open OUnit2

let suite = "zeit" >::: [Test_deployment.suite; Test_client.suite]

let () = run_test_tt_main suite
