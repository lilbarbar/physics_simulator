open! Core
open! Objects
open! World_graphics

(* open! State *)

type t =
  { balls : Ball.t list
  ; lines : Line.t list
  ; cups : Cup.t list
  ; world_state : State.t
  }

let draw_all_balls t =
  List.iter t.balls ~f:(fun ball -> World_graphics.draw_ball ball)
;;
