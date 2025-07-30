open! Core
open! Async
open! Graphics

let on_button_click (t : World.t) id =
  match id with
  | "create-ball-btn" ->
    t.click_state <- Click_state.Create_object_select_first Objects.Ball
  | "create-line-btn" ->
    t.click_state <- Click_state.Create_object_select_first Objects.Line
  | "create-cup-btn" ->
    t.click_state <- Click_state.Create_object_select_first Objects.Cup
  | _ -> ()
;;

let handle_select_object_first (t : World.t) (obj : Objects.t) x y =
  print_endline "handle_create_object";
  let first_selected_pos =
    { Vector.x = Float.of_int x; y = Float.of_int y }
  in
  if Interface.Canvas.in_bounds t.ui.canvas x y
  then (
    match obj with
    | Objects.Ball ->
      t.click_state
      <- Click_state.Create_object_select_final
           (Objects.Ball, first_selected_pos)
    | Objects.Box ->
      t.click_state
      <- Click_state.Create_object_select_final
           (Objects.Box, first_selected_pos)
    | Objects.Line ->
      t.click_state
      <- Click_state.Create_object_select_final
           (Objects.Ball, first_selected_pos)
    | Objects.Cup ->
      t.click_state
      <- Click_state.Create_object_select_final
           (Objects.Ball, first_selected_pos))
;;

let handle_select_object_final (t : World.t) obj first_selected_pos x y =
  let second_selected_pos =
    { Vector.x = Float.of_int x; y = Float.of_int y }
  in
  let dist = Vector.dist first_selected_pos second_selected_pos in
  if Interface.Canvas.in_bounds t.ui.canvas x y
  then (
    match obj with
    | Objects.Ball ->
      let new_ball =
        Objects.Ball.create ~center:first_selected_pos ~mass:10.0 ~radius:dist
      in
      Interface.Canvas.add_ball t.ui.canvas new_ball
    | Objects.Cup ->
      let new_cup = Objects.Cup.create ~min:first_selected_pos ~max:second_selected_pos in
      Interface.Canvas.add_cup t.ui.canvas new_cup
    | Objects.Box ->
      let new_box =
        Objects.Box.create ~min:first_selected_pos ~max:second_selected_pos ~mass:10.0
      in
      Interface.Canvas.add_box t.ui.canvas new_box
    | Objects.Line ->
      let new_line =
        Objects.Line.create ~first_endp:first_selected_pos ~second_endp:second_selected_pos
      in
      Interface.Canvas.add_line t.ui.canvas new_line)
;;

let handle_drag_object t obj x y =
  ignore t;
  ignore obj
;;

let handle_select_object t obj x y =
  ignore t;
  ignore obj
;;

let handle_free_state (t : World.t) x y =
  print_endline "handle_free_state";
  let buttons = t.ui.panel.buttons in
  List.iter buttons ~f:(fun button ->
    if Interface.Button.in_bounds button x y then on_button_click t button.id)
;;

let rec handle_click (t : World.t) : unit Deferred.t =
  let%bind event =
    In_thread.run (fun () ->
      let event = wait_next_event [ Button_down ] in
      print_endline "Click down";
      event)
  in
  if event.button
  then (
    let x = event.mouse_x in
    let y = event.mouse_y in
    (match t.click_state with
     | Click_state.Create_object_select_first obj ->
       handle_select_object_first t obj x y
     | Click_state.Create_object_select_final (obj, first_selected_pos) ->
       handle_select_object_final t obj first_selected_pos x y
     | Click_state.Drag_current_object obj -> handle_drag_object t obj x y
     | Click_state.Select_current_object obj ->
       handle_select_object t obj x y
     | Click_state.Free_state -> handle_free_state t x y
     | _ -> ());
    handle_click t)
  else handle_click t
;;
