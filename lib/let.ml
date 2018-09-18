module Json = struct
  module Let_syntax = struct
    let map ~f x = Ppx_deriving_yojson_runtime.( >|= ) x f
  end
end

module Lwt = struct
  module Let_syntax = struct
    let bind ~f x = Lwt.bind x f

    let map ~f x = Lwt.map f x
  end
end
