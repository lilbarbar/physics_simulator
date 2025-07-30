open! Core
open! Async
open! Graphics

val on_button_click : World.t -> string -> unit
val handle_create_object : World.t -> Objects.t -> int -> int -> unit
val handle_drag_object : World.t -> Objects.t -> int -> int -> unit
val handle_select_object : World.t -> Objects.t -> int -> int -> unit
val handle_free_state : World.t -> int -> int -> unit
val handle_click : World.t -> unit Deferred.t
