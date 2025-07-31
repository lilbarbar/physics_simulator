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
  | "create-box-btn" ->
    t.click_state <- Click_state.Create_object_select_first Objects.Box
  | "clear-btn" ->
    Interface.Canvas.clear t.ui.canvas;
    t.click_state <- Click_state.Free_state
  | _ -> ()
;;

let handle_select_object_first (t : World.t) (obj : Objects.t) x y =
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
           (Objects.Line, first_selected_pos)
    | Objects.Cup ->
      t.click_state
      <- Click_state.Create_object_select_final
           (Objects.Cup, first_selected_pos))
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
    Objects.find_min_max first_selected_pos second_selected_pos
  in
  if Interface.Canvas.in_bounds t.ui.canvas x y
  then (
    match obj with
    | Objects.Ball ->
      let new_ball =
        Objects.Ball.create
          ~center:first_selected_pos
          ~mass:10.0
          ~radius:dist
      in
      Interface.Canvas.add_ball t.ui.canvas new_ball;
      t.click_state <- Click_state.Create_object_select_first Objects.Ball
    | Objects.Cup ->
      let new_cup = Objects.Cup.create ~min:min_pos ~max:max_pos in
      Interface.Canvas.add_cup t.ui.canvas new_cup;
      t.click_state <- Click_state.Create_object_select_first Objects.Cup
    | Objects.Box ->
      let new_box =
        Objects.Box.create ~min:min_pos ~max:max_pos ~mass:10.0
      in
      Interface.Canvas.add_box t.ui.canvas new_box;
      t.click_state <- Click_state.Create_object_select_first Objects.Box
    | Objects.Line ->
      let new_line =
        Objects.Line.create
          ~first_endp:first_selected_pos
          ~second_endp:second_selected_pos
      in
      Interface.Canvas.add_line t.ui.canvas new_line;
      t.click_state <- Click_state.Create_object_select_first Objects.Line)
  else
    List.iter t.ui.panel.buttons ~f:(fun button ->
      if Interface.Button.in_bounds button x y
      then on_button_click t button.id)
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
  let buttons = t.ui.panel.buttons in
  List.iter buttons ~f:(fun button ->
    if Interface.Button.in_bounds button x y then on_button_click t button.id);
  List.iter t.ui.canvas.balls ~f:(fun ball -> 
    
    )
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
     | Click_state.Free_state -> handle_free_state t x y
     | _ -> ());
    handle_click t)
  else (
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
;;
