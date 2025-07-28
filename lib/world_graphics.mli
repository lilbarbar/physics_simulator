open! Core
open! Graphics
open! Objects

val draw_ball : Ball.t -> unit
val draw_line : Line.t -> unit
val draw_cup : Cup.t -> unit
val create_environment : ?width:int -> ?height:int -> unit -> unit
val init_exn : unit -> unit
val render : World.t -> unit
