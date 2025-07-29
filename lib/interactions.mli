open! Async

module Click_state : sig
  type t =
    | Create_new_object of Type_of_object.t
    | Drag_current_object of Type_of_object.t
    | Select_current_object of Type_of_object.t
    | Free
end

(* val handle_click : World.t -> unit Deferred.t *)