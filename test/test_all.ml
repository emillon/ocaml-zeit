open OUnit2

let suite = "zeit" >::: [Test_deployment.suite]

let () = run_test_tt_main suite
