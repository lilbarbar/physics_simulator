open! Core

type t =
  { x : float
  ; y : float
  }
[@@deriving compare, equal, sexp_of]

val to_string : t -> string
val list_to_string : t list -> string
val of_x_major_coord : float * float -> t
val of_x_major_coords : (float * float) list -> t list
val ( + ) : t -> t -> t
val ( - ) : t -> t -> t
val multi_sum : t list -> t
val dist_squared : t -> t -> float
val dist : t -> t -> float
val dot_product : t -> t -> float
val scale : t -> k:float -> t
val mag_squared : t -> float
val mag : t -> float
val angle_between : t -> t -> float
val normalize : t -> t
val is_zero : t -> bool
val rotate : t -> theta:float -> t
