module Json = struct
  module Let_syntax = struct
    let map ~f x = Ppx_deriving_yojson_runtime.( >|= ) x f
  end
end

module Lwt_result = struct
  module Let_syntax = struct
    let bind ~f x =
      Lwt.bind x (function
        | Ok y ->
            f y
        | Error e ->
            Lwt.return_error e )


    let map ~f x =
      Lwt.map
        (function
          | Ok y ->
              Ok (f y)
          | Error _ as e ->
              e)
        x
  end
end

module Lwt = struct
  module Let_syntax = struct
    let bind ~f x = Lwt.bind x f

    let map ~f x = Lwt.map f x
  end
end
