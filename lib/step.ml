open! Core
open! Graphics
open! Objects

let step_click_state_display_text (t : World.t) dt =
  ignore dt;
  List.iter t.ui.panel.text_boxes ~f:(fun text_box ->
    if String.equal text_box.id "click_state_text"
    then text_box.display_text <- Click_state.to_string t.click_state)
;;

let drag_box_cup_object current_mouse_pos (min : Vector.t) (max : Vector.t) =
  let width = max.x -. min.x in
  let height = max.y -. min.y in
  let new_min_pos =
    Vector.translate_xy current_mouse_pos (width /. -2.0) (height /. -2.0)
  in
  let new_max_pos =
    Vector.translate_xy current_mouse_pos (width /. 2.0) (height /. 2.0)
  in
  new_min_pos, new_max_pos
;;

let step_drag_object (t : World.t) dt =
  ignore dt;
  let mouse_x, mouse_y = mouse_pos () in
  let current_mouse_pos =
    { Vector.x = Float.of_int mouse_x; y = Float.of_int mouse_y }
  in
  match t.click_state with
  | Click_state.Drag_current_object obj ->
    (match obj with
     | Ball ball -> ball.center <- current_mouse_pos
     | Box box ->
       let new_min_pos, new_max_pos =
         drag_box_cup_object current_mouse_pos box.min box.max
       in
       box.min <- new_min_pos;
       box.max <- new_max_pos
     | Line line ->
       let line_center = Objects.Line.center line in
       let vector_to_first_endp = Vector.( - ) line.first_endp line_center in
       let vector_to_second_endp =
         Vector.( - ) line.second_endp line_center
       in
       let new_first_endp =
         Vector.( + ) current_mouse_pos vector_to_first_endp
       in
       let new_second_endp =
         Vector.( + ) current_mouse_pos vector_to_second_endp
       in
       line.first_endp <- new_first_endp;
       line.second_endp <- new_second_endp
     | Cup cup ->
       let new_min_pos, new_max_pos =
         drag_box_cup_object current_mouse_pos cup.min cup.max
       in
       cup.min <- new_min_pos;
       cup.max <- new_max_pos)
  | _ -> ()
;;

let step_positions (t : World.t) dt =
  List.iter t.ui.canvas.balls ~f:(fun ball -> Ball.update_pos ball dt);
  Canvas.bound_objects t.ui.canvas
;;

let step_velocities (t : World.t) dt =
  List.iter t.ui.canvas.balls ~f:(fun ball -> Ball.update_vel ball dt)
;;

let step_reactions (t : World.t) dt =
  ignore dt;
  Reactions.update_forces t.ui.canvas
;;

let step (t : World.t) dt =
  if
    World_state.equal t.world_state World_state.In_progress
    && Click_state.equal t.click_state Click_state.Free_state
  then (
    step_reactions t dt;
    step_velocities t dt;
    step_positions t dt);
  step_drag_object t dt;
  step_click_state_display_text t dt
;;
