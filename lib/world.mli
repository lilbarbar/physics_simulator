open! Core
open! Objects

type t =
  { balls : Ball.t list
  ; lines : Line.t list
  ; world_state : World_state.t
  }

val create : unit -> t
val world_state : t -> World_state.t
val step : t -> unit
