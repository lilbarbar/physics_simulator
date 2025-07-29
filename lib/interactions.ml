open! Async
open! Graphics
open! Core

module Click_state = struct
  type t =
    | Create_new_object of Type_of_object.t
    | Drag_current_object of Type_of_object.t
    | Select_current_object of Type_of_object.t
    | Free
end
(* 
let rec handle_click old_t : unit Deferred.t =
  let ovr_width = 750 in
  let ovr_height = 500 in
  let%bind event =
    In_thread.run (fun () ->
      let event = wait_next_event [ Button_down ] in
      event)
  in
  if event.button
  then (
    let object_type = old_t.current_object in
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
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
          ; mass = 100
          ; forces = []
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
      | _ -> handle_clicks old_t)
    else if
      click_x >= 7 * ovr_width / 10
      && click_x < (7 * ovr_width / 10) + (1 * ovr_width / 15)
      && click_y > 9 * ovr_width / 10
      && click_y < (9 * ovr_width / 10) + (1 * ovr_height / 20)
    then (
      let new_t =
        generate_next_frame { old_t with current_object = New_ball }
      in
      draw_everything new_t;
      handle_clicks new_t)
    else if
      click_x >= 8 * ovr_width / 10
      && click_x < (7 * ovr_width / 10) + (1 * ovr_width / 15)
      && click_y > 9 * ovr_width / 10
      && click_y < (9 * ovr_width / 10) + (1 * ovr_height / 20)
    then (
      let new_t =
        generate_next_frame { old_t with current_object = New_line }
      in
      draw_everything new_t;
      handle_clicks new_t)
    else if
      click_x >= 9 * ovr_width / 10
      && click_x < (7 * ovr_width / 10) + (1 * ovr_width / 15)
      && click_y > 9 * ovr_width / 10
      && click_y < (9 * ovr_width / 10) + (1 * ovr_height / 20)
    then (
      let new_t =
        generate_next_frame { old_t with current_object = New_cup }
      in
      draw_everything new_t;
      handle_clicks new_t)
    else (
      let new_t = generate_next_frame old_t in
      draw_everything new_t;
      handle_clicks new_t))
  else (
    let new_t = generate_next_frame old_t in
    draw_everything new_t;
    print_string "lol";
    handle_clicks new_t)
;; *)
