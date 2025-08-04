open! Core
open! Objects

val ball_line_force_interaction : Ball.t -> Line.t -> unit
val ball_cup_force_interaction : Ball.t -> Cup.t -> unit
val all_ball_and_line_forces : Canvas.t -> unit
val all_ball_and_cup_forces : Canvas.t -> unit
val update_forces : Canvas.t -> unit