open! Core
open! Objects
open! Async
open! Graphics

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

let step_drag_object t =
  let mouse_x, mouse_y = mouse_pos () in
  let current_mouse_pos =
    { Vector.x = Float.of_int mouse_x; y = Float.of_int mouse_y }
  in
  match t.click_state with
  | Click_state.Drag_current_object obj ->
    (match obj with
     | Ball ball -> ball.center <- current_mouse_pos
     | Box box ->
       let box_width = box.max.x -. box.min.x in
       let box_height = box.max.y -. box.min.y in
       let new_min_pos =
         Vector.translate_xy
           current_mouse_pos
           (box_width /. -2.0)
           (box_height /. -2.0)
       in
       let new_max_pos =
         Vector.translate_xy
           current_mouse_pos
           (box_width /. 2.0)
           (box_height /. 2.0)
       in
       box.min <- new_min_pos;
       box.max <- new_max_pos
     | _ -> ())
  | _ -> ()
;;

let step t = step_drag_object t
