module Json : sig
  module Let_syntax : sig
    val map : f:('a -> 'b) -> ('a, string) result -> ('b, string) result
  end
end

module Lwt_result : sig
  module Let_syntax : sig
    val bind :
         f:('a -> ('b, Error.t) result Lwt.t)
      -> ('a, Error.t) result Lwt.t
      -> ('b, Error.t) result Lwt.t

    val map :
      f:('a -> 'b) -> ('a, Error.t) result Lwt.t -> ('b, Error.t) result Lwt.t
  end
end

module Lwt : sig
  module Let_syntax : sig
    val bind : f:('a -> 'b Lwt.t) -> 'a Lwt.t -> 'b Lwt.t

    val map : f:('a -> 'b) -> 'a Lwt.t -> 'b Lwt.t
  end
end
