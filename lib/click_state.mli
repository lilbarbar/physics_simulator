open! Core

type t =
  | Create_object_select_first of Objects.t
  | Create_object_select_final of (Objects.t * Vector.t)
  | Drag_current_object of Objects.ObjectSelector.t
  | Select_current_object of Objects.t
  | Free_state

val equal : t -> t -> bool
