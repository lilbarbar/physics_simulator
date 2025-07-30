(* open! Core
open Async
open! Objects
open! World_graphics
open! Type_of_object
open! Graphics

(* type t =
  { balls : Ball.t list
  ; lines : Line.t list
  ; cups : Cup.t list
  ; world_state : State.t
  ; click_interactions : Click_interactions.t
  } *)

(* let create_world ?(world_width = 750) ?(world_height = 500) () : t =
  World_graphics.create_enviornment ();
  { balls = []
  ; lines = []
  ; cups = []
  ; world_state = Paused
  ; current_object = New_cup
  }
;; *)

(* let generate_next_frame t : t =
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
  ; current_object = t.current_object
  }
;; *)

let handle_create_object t obj event =
  ignore t;
  ignore obj
;;

let handle_drag_object t obj event =
  ignore t;
  ignore obj
;;

let handle_select_object t obj event =
  ignore t;
  ignore obj
;;

let handle_free_state t event = 
  ignore t;
  ignore event
;;

let rec handle_click (t : World.t) : unit Deferred.t =
  let%bind event =
    In_thread.run (fun () ->
      let event = wait_next_event [ Button_down ] in
      event)
  in
  if event.button
  then (
    (match t.click_state with
     | Create_new_object obj -> handle_create_object t obj event
     | Drag_current_object obj -> handle_drag_object t obj event
     | Select_current_object obj -> handle_select_object t obj event
     | Free_state -> handle_free_state t event);
    handle_click t)
  else handle_click t
;;

then
    (
    let object_type = old_t.current_object in
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
    (* Example: If click is within a specific region, draw a circle *)
    if click_x >= 0 && click_x < 500 && click_y >= 0 && click_y <= 500
    then (
      print_s [%message "object type is" (object_type : Type_of_object.t)];
      match object_type with
      | New_ball ->
        let new_ball : Ball.t =
          { x_pos = click_x
          ; y_pos = click_y
          ; x_vel = 0
          ; y_vel = 0
          ; mass = 1
          ; forces =
              [ { magnitude = 9.8
                ; x_direction = 0.0
                ; y_direction = -1.0
                ; name = "Gravity"
                }
              ]
          }
        in
        let new_t =
          generate_next_frame { old_t with balls = new_ball :: old_t.balls }
        in
        draw_everything new_t;
        handle_clicks new_t
      | New_cup ->
        let new_cup : Cup.t = { x_pos = click_x; y_pos = click_y } in
        let new_t =
          generate_next_frame { old_t with cups = new_cup :: old_t.cups }
        in
        draw_everything new_t;
        handle_clicks new_t
      | New_line ->
        let x1 = click_x in
        let y1 = click_y in
        let x2, y2 = second_line_point () in
        let new_line : Line.t =
          { x1_pos = x1
          ; x2_pos = x2
          ; y1_pos = y1
          ; y2_pos = y2
          ; friction_constant = 0.0
          }
        in
        let new_t =
          generate_next_frame { old_t with lines = new_line :: old_t.lines }
        in
        draw_everything new_t;
        handle_clicks new_t
      | _ -> handle_clicks old_t)
    else if
      click_x >= 525
      (* 7 * ovr_width / 10 *)
      && click_x < 575
      (* (7 * ovr_width / 10) + (1 * ovr_width / 15) *)
      && click_y > 450
      (* 9 * ovr_width / 10 *)
      && click_y < 475
      (* (9 * ovr_width / 10) + (1 * ovr_height / 20) *)
    then (
      (* create_screen (); *)
      let new_t =
        generate_next_frame { old_t with current_object = New_ball }
      in
      print_string "Now is a ball!";
      draw_everything new_t;
      handle_clicks new_t)
    else if click_x >= 600 && click_x < 650 && click_y > 450 && click_y < 475
    then (
      (* create_screen (); *)
      let new_t =
        generate_next_frame { old_t with current_object = New_line }
      in
      draw_everything new_t;
      handle_clicks new_t)
    else if click_x >= 675 && click_x < 725 && click_y > 450 && click_y < 475
    then (
      (* create_screen (); *)
      let new_t =
        generate_next_frame { old_t with current_object = New_cup }
      in
      draw_everything new_t;
      handle_clicks new_t)
    else (
      let new_t = generate_next_frame old_t in
      draw_everything new_t;
      handle_clicks new_t))
  (* Continue handling clicks *)
  else (
    let new_t = generate_next_frame old_t in
    draw_everything new_t;
    print_string "lol";
    handle_clicks new_t (* If no button down, continue waiting for clicks *))
;; *)
