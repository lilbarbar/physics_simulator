open! Core

type t =
  | Create_object_now of Objects.t
  | Drag_current_object of Objects.t
  | Select_current_object of Objects.t
  | Free_state
