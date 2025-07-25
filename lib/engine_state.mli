open! Core

(** A [t] represents the current state of the game. *)
type t =
  | In_progress
  | Paused
  | Clear
  | Failure
[@@deriving sexp_of, compare]

(** [to_string] pretty-prints the current game state into a string. *)
val to_string : t -> string
