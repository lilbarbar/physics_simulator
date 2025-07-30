open! Core
open! Graphics
open! Objects
open! Async

val draw_ball : Ball.t -> unit
val draw_line : Line.t -> unit
val draw_cup : Cup.t -> unit
val create_environment : ?width:int -> ?height:int -> unit -> unit
val init_exn : Interface.UI.t -> unit
val render : World.t -> unit
