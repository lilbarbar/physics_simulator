open! Core
open! Objects
open! World_graphics

type t =
  { balls : Ball.t list
  ; lines : Line.t list
  ; cups : Cup.t list
  ; world_state : Engine_state.t
  ; canvas : Canvas.t
  }
[@@deriving sexp_of]

let draw_all_balls t =
  List.iter t.balls ~f:(fun ball -> World_graphics.draw_ball ball)
;;

let draw_all_lines t =
  List.iter t.lines ~f:(fun line -> World_graphics.draw_line line)
;;

let draw_all_cups t =
  List.iter t.cups ~f:(fun cup -> World_graphics.draw_cup cup)
;;

let draw_everything t =
  draw_all_balls t;
  draw_all_lines t;
  draw_all_cups t
;;

let gen_next_step t : t =
  let new_balls =
    List.map t.balls ~f:(fun ball ->
      let new_ball : Ball.t =
        { x_pos = Ball.next_x ball
        ; y_pos = Ball.next_y ball
        ; x_vel = Ball.next_v_x ball
        ; y_vel = Ball.next_v_y ball
        ; mass = ball.mass
        ; forces = ball.forces
        }
      in
      new_ball)
  in
  { balls = new_balls
  ; lines = t.lines
  ; cups = t.cups
  ; world_state = t.world_state
  ; canvas = canvas
  }
;;

let create ~height ~width ~initial_snake_length =
  let canvas = Canvas.create ~height ~width in
  { engine_state = In_progress; canvas }
;;

let to_string { engine_state; canvas } =
  Core.sprintf
    !{| %{sexp:Engine_state.t}
%{sexp:Canvas.t}
%s |}
    engine_state
    canvas
;;
