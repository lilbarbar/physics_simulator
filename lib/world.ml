open! Core
open! Objects

type t =
  { balls : Ball.t list
  ; lines : Line.t list
  ; world_state : World_state.t
  }

let create () = { world_state = In_progress; balls = []; lines = [] }
let world_state t = t.world_state
let step t = ignore t
