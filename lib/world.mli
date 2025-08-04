open! Core
open! Objects
open! Async

type t =
  { mutable world_state : World_state.t
  ; mutable ui : Interface.UI.t
  ; mutable click_state : Click_state.t
  }

val create : unit -> t
val step : t -> float -> unit
