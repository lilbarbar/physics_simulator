open! Core

type t =
  | In_progress
  | Paused
  | Clear
  | Failure
[@@deriving sexp_of, compare]

val to_string : t -> string