open! Core

type t =
  | Create_object_now of Type_of_object.t
  | Drag_current_object of Type_of_object.t
  | Select_current_object of Type_of_object.t
  | Free_state