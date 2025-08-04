open! Core
open! Async
open! Graphics
open! Objects

let handle_state_on_button_click (t : World.t) btn_click_state id =
  if Click_state.equal t.click_state btn_click_state
  then t.click_state <- Click_state.Free_state
  else t.click_state <- btn_click_state
;;

let on_button_click (t : World.t) id =
  match id with
  | "create-ball-btn" ->
    let create_ball_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Ball
    in
    handle_state_on_button_click t create_ball_btn_state id
  | "create-line-btn" ->
    let create_line_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Line
    in
    handle_state_on_button_click t create_line_btn_state id
  | "create-cup-btn" ->
    let create_cup_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Cup
    in
    handle_state_on_button_click t create_cup_btn_state id
  | "create-box-btn" ->
    let create_box_btn_state =
      Click_state.Create_object_select_first ObjectTypeSelector.Box
    in
    handle_state_on_button_click t create_box_btn_state id
  | "clear-btn" ->
    Interface.Canvas.clear t.ui.canvas;
    t.click_state <- Click_state.Free_state
  | _ -> ()
;;

let handle_select_object_first (t : World.t) (obj : ObjectTypeSelector.t) x y
  =
  let first_selected_pos =
    { Vector.x = Float.of_int x; y = Float.of_int y }
  in
  if Interface.Canvas.in_bounds t.ui.canvas x y
  then (
    match obj with
    | ObjectTypeSelector.Ball ->
      t.click_state
      <- Click_state.Create_object_select_final
           (ObjectTypeSelector.Ball, first_selected_pos)
    | ObjectTypeSelector.Box ->
      t.click_state
      <- Click_state.Create_object_select_final
           (ObjectTypeSelector.Box, first_selected_pos)
    | ObjectTypeSelector.Line ->
      t.click_state
      <- Click_state.Create_object_select_final
           (ObjectTypeSelector.Line, first_selected_pos)
    | ObjectTypeSelector.Cup ->
      t.click_state
      <- Click_state.Create_object_select_final
           (ObjectTypeSelector.Cup, first_selected_pos))
  else
    List.iter t.ui.panel.buttons ~f:(fun button ->
      if Interface.Button.in_bounds button x y
      then on_button_click t button.id)
;;

let handle_select_object_final (t : World.t) obj first_selected_pos x y =
  let second_selected_pos =
    { Vector.x = Float.of_int x; y = Float.of_int y }
  in
  let dist = Vector.dist first_selected_pos second_selected_pos in
  let min_pos, max_pos =
    find_min_max first_selected_pos second_selected_pos
  in
  if Interface.Canvas.in_bounds t.ui.canvas x y
  then (
    match obj with
    | ObjectTypeSelector.Ball ->
      let new_ball =
        Ball.create ~center:first_selected_pos ~mass:10.0 ~radius:dist
      in
      Interface.Canvas.add_ball t.ui.canvas new_ball;
      t.click_state <- Click_state.Free_state
    | ObjectTypeSelector.Cup ->
      let new_cup = Objects.Cup.create ~min:min_pos ~max:max_pos in
      Interface.Canvas.add_cup t.ui.canvas new_cup;
      t.click_state <- Click_state.Free_state
    | ObjectTypeSelector.Box ->
      let new_box =
        Objects.Box.create ~min:min_pos ~max:max_pos ~mass:10.0
      in
      Interface.Canvas.add_box t.ui.canvas new_box;
      t.click_state <- Click_state.Free_state
    | ObjectTypeSelector.Line ->
      let new_line =
        Line.create
          ~first_endp:first_selected_pos
          ~second_endp:second_selected_pos
      in
      Interface.Canvas.add_line t.ui.canvas new_line;
      t.click_state <- Click_state.Free_state)
  else
    List.iter t.ui.panel.buttons ~f:(fun button ->
      if Interface.Button.in_bounds button x y
      then on_button_click t button.id)
;;

let handle_drag_object (t : World.t) obj x y =
  t.click_state <- Click_state.Free_state
;;

let handle_select_object t obj x y =
  ignore t;
  ignore obj
;;

let handle_free_state (t : World.t) x y =
  let buttons = t.ui.panel.buttons in
  List.iter buttons ~f:(fun button ->
    if Interface.Button.in_bounds button x y then on_button_click t button.id);
  List.iter t.ui.canvas.balls ~f:(fun ball ->
    let point = { Vector.x = Float.of_int x; y = Float.of_int y } in
    if Collisions.ball_point_collide ball point
    then
      t.click_state
      <- Click_state.Drag_current_object ObjectSelector.(Ball ball));
  List.iter t.ui.canvas.boxes ~f:(fun box ->
    let point = { Vector.x = Float.of_int x; y = Float.of_int y } in
    if Collisions.box_point_collide box point
    then
      t.click_state
      <- Click_state.Drag_current_object ObjectSelector.(Box box));
  List.iter t.ui.canvas.lines ~f:(fun line ->
    let point = { Vector.x = Float.of_int x; y = Float.of_int y } in
    if Collisions.line_point_collide line point
    then
      t.click_state
      <- Click_state.Drag_current_object ObjectSelector.(Line line));
  List.iter t.ui.canvas.cups ~f:(fun cup ->
    let point = { Vector.x = Float.of_int x; y = Float.of_int y } in
    if Collisions.cup_point_collide cup point
    then
      t.click_state
      <- Click_state.Drag_current_object ObjectSelector.(Cup cup))
;;

let rec handle_click (t : World.t) : unit Deferred.t =
  let%bind event =
    In_thread.run (fun () ->
      let event = wait_next_event [ Button_down; Button_up ] in
      event)
  in
  let x = event.mouse_x in
  let y = event.mouse_y in
  if event.button
  then (
    (match t.click_state with
     | Click_state.Create_object_select_first obj ->
       handle_select_object_first t obj x y
     | Click_state.Create_object_select_final (obj, first_selected_pos) ->
       handle_select_object_final t obj first_selected_pos x y
     | Click_state.Drag_current_object obj -> handle_drag_object t obj x y
     | Click_state.Select_current_object obj ->
       handle_select_object t obj x y
     | Click_state.Free_state -> handle_free_state t x y);
    handle_click t)
  else (
    (match t.click_state with
     | Click_state.Drag_current_object obj -> handle_drag_object t obj x y
     | _ -> ());
    handle_click t)
;;
