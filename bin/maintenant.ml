let run_list_deployments token =
  let open Zeit.Let.Lwt in
  let client = Zeit.Client.make ~token () in
  Lwt_main.run
    ( match%bind Zeit.Client.list_deployments client with
    | Ok deployments ->
        List.iter (Format.printf "%a%!\n" Zeit.Deployment.pp) deployments;
        Lwt.return_unit
    | Error e ->
        Lwt.fail_with (Zeit.Error.to_string e) )


let token =
  let open Cmdliner.Arg in
  let env = env_var "MAINTENANT_TOKEN" in
  let info = info ~env ["token"] in
  required (opt (some string) None info)


let help =
  let open Cmdliner.Term in
  (ret (const (`Help (`Auto, None))), info "maintenant")


let list_deployments =
  let open Cmdliner.Term in
  (const run_list_deployments $ token, info ~doc:"List all deployments" "list")


let () =
  let open Cmdliner.Term in
  exit @@ eval_choice help [list_deployments]
