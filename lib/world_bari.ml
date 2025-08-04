open! Core
open! Objects
open! Async
open! Collisions
open! Reactions

type t =
  { mutable world_state : World_state.t
  ; mutable ui : Interface.UI.t
  ; mutable click_state : Click_state.t
  }

let create () =
  { world_state = In_progress
  ; ui = Interface.UI.create ~height:500 ~width:750
  ; click_state = Click_state.Free_state
  }
;;

let step_forces t = update_forces t.ui.canvas

let step t =
  (* step_forces t; *)
  List.iter t.ui.canvas.balls ~f:(fun ball ->
    Ball.update_vel ball 0.1;
    Ball.update_pos ball 0.1);
  print_string "Step Done";
  print_s [%sexp (t.ui.canvas.balls : Ball.t list)]
;;
