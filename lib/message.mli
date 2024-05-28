type t [@@deriving show]

val gen_key : t -> Digestif.SHA1.t
