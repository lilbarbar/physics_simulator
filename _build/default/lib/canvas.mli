open! Core

type t [@@deriving sexp_of]

val create : height:int -> width:int -> t
val create_unlabeled : int -> int -> t

val in_bounds : t -> Position.t -> bool

val all_positions : t -> Position.t list