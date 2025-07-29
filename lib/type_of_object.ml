open! Core
open! Objects

type t =
  | Ball of Ball.t
  | Line of Line.t
  | Cup of Cup.t
  | Box of Box.t
