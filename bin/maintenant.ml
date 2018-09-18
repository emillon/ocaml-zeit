let run t =
  match Lwt_main.run t with
  | Ok x ->
      x
  | Error e ->
      failwith (Zeit.Error.to_string e)


let run_list_deployments token =
  let open Zeit.Let.Lwt_result in
  let client = Zeit.Client.make ~token () in
  run
    (let%map deployments = Zeit.Client.list_deployments client in
     List.iter (Format.printf "%a%!\n" Zeit.Deployment.pp) deployments)


let run_deploy_sample token name =
  let index_html = "<h1>Hello, world!</h1>" in
  let client = Zeit.Client.make ~token () in
  let open Zeit.Let.Lwt_result in
  run
    (let%bind sha1 = Zeit.Client.post_file client index_html in
     let file = ("index.html", sha1, String.length index_html) in
     let%map deploy =
       Zeit.Client.create_deployment client ~name ~files:[file]
     in
     print_endline @@ Zeit.Deployment.Api_responses.show_create_result deploy)


let arg_token =
  let open Cmdliner.Arg in
  let env = env_var "MAINTENANT_TOKEN" in
  let info = info ~env ["token"] in
  required (opt (some string) None info)


let arg_name =
  let open Cmdliner.Arg in
  let info = info ["name"] in
  required (opt (some string) None info)


let help =
  let open Cmdliner.Term in
  (ret (const (`Help (`Auto, None))), info "maintenant")


let list_deployments =
  let open Cmdliner.Term in
  ( const run_list_deployments $ arg_token
  , info ~doc:"List all deployments" "list" )


let deploy_sample =
  let open Cmdliner.Term in
  ( const run_deploy_sample $ arg_token $ arg_name
  , info ~doc:"Deploy a sample project" "deploy-sample" )


let () =
  let open Cmdliner.Term in
  exit @@ eval_choice help [list_deployments; deploy_sample]
