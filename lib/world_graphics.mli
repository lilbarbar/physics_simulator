open! Core
open! Graphics
open! Objects
open! Async

val draw_ball : Ball.t -> unit
val draw_line : Line.t -> unit
val draw_cup : Cup.t -> unit
val draw_box : Box.t -> unit
val draw_objects : Interface.UI.t -> unit
val draw_panel : Interface.UI.t -> unit
val create_environment : Interface.UI.t -> unit
val init_exn : Interface.UI.t -> unit
val render : Interface.UI.t -> unit
