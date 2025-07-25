open! Core

(** A [t] represents the entire game state, including the current snake, apple,
    and game state. *)
type t [@@deriving sexp_of]

(** Used for pretty-printing game contents for tests. *)
val to_string : t -> string

(** [create] creates a new game with specified parameters. *)
val create : height:int -> width:int -> initial_snake_length:int -> t

(** [game_state] returns the state of the current game. *)
val engine_state : t -> Engine_state.t

val step : t -> unit
