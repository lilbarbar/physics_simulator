open! Core

type t =
  | Create_object_now of Objects.t
  | Create_object_select_first of Objects.t
  | Create_object_select_final of (Objects.t * Vector.t)
  | Drag_current_object of Objects.ObjectSelector.t
  | Select_current_object of Objects.t
  | Free_state

let equal (a : t) (b : t) : bool =
  match a, b with
  | Free_state, Free_state -> true
  | Create_object_now o1, Create_object_now o2 -> true
  | Create_object_select_first o1, Create_object_select_first o2 -> true
  | Create_object_select_final (o1, v1), Create_object_select_final (o2, v2)
    ->
    true
  | Drag_current_object s1, Drag_current_object s2 -> true
  | Select_current_object o1, Select_current_object o2 -> true
  | _, _ -> false
;;
