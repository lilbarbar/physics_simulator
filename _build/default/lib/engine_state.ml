open! Core

type t =
  | In_progress
  | Paused
  | Clear
  | Failure
[@@deriving sexp_of, compare]

let to_string t =
  match t with
  | In_progress -> "in-progress"
  | Paused -> "paused"
  | Clear -> "clear"
  | Failure -> "failure"
;;
