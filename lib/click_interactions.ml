open! Core
open! Async
open! Graphics

let on_button_click (t : World.t) id =
  match id with
  | "create-ball-btn" ->
    t.click_state <- Click_state.Create_object_now Objects.Ball
  | "create-line-btn" ->
    t.click_state <- Click_state.Create_object_now Objects.Line
  | "create-cup-btn" ->
    t.click_state <- Click_state.Create_object_now Objects.Cup
  | _ -> ()
;;

let handle_create_object (t : World.t) (obj : Objects.t) x y =
  print_endline "handle_create_object";
  let create_at = { Vector.x = Float.of_int x; y = Float.of_int y } in
  let create_at_max = Vector.( + ) { Vector.x = 30.0; y = 30.0 } create_at in
  if Interface.Canvas.in_bounds t.ui.canvas x y
  then (
    match obj with
    | Ball ->
      let new_ball =
        Objects.Ball.create ~center:create_at ~mass:10.0 ~radius:30.0
      in
      Interface.Canvas.add_ball t.ui.canvas new_ball
    | Cup ->
      let new_cup = Objects.Cup.create ~min:create_at ~max:create_at_max in
      Interface.Canvas.add_cup t.ui.canvas new_cup
    | Box ->
      let new_box =
        Objects.Box.create ~min:create_at ~max:create_at_max ~mass:10.0
      in
      Interface.Canvas.add_box t.ui.canvas new_box
    | Line ->
      let new_line =
        Objects.Line.create ~first_endp:create_at ~second_endp:create_at_max
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
     | Click_state.Create_object_now obj -> handle_create_object t obj x y
     | Click_state.Drag_current_object obj -> handle_drag_object t obj x y
     | Click_state.Select_current_object obj ->
       handle_select_object t obj x y
     | Click_state.Free_state -> handle_free_state t x y);
    handle_click t)
  else handle_click t
;;
