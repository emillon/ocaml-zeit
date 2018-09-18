type t

val make :
     ?cohttp_get:(   Cohttp.Header.t
                  -> Uri.t
                  -> (Cohttp.Response.t * Cohttp_lwt.Body.t) Lwt.t)
  -> token:string
  -> unit
  -> t

val list_deployments : t -> (Deployment.t list, Error.t) result Lwt.t
