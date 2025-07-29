open! Core
open! Objects
open! Async

type t =
  { mutable balls : Ball.t list
  ; mutable lines : Line.t list
  ; mutable cups : Cup.t list
  ; mutable world_state : World_state.t
  ; mutable click_state : Click_state.t
  }

val create : unit -> t
val world_state : t -> World_state.t
val step : t -> unit
