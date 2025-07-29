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

let create () = { world_state = In_progress; balls = []; lines = [] }
let world_state t = t.world_state
let step t = ignore t
