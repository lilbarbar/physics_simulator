open! Core

module Ball : sig
  type t =
    { mutable center : Vector.t
    ; mutable velocity : Vector.t
    ; mutable net_force : Vector.t
    ; mass : float
    ; radius : float
    }

  val update_pos : t -> float -> unit
  val update_vel : t -> float -> unit
  val set_vel : t -> Vector.t -> unit
  val add_vel : t -> Vector.t -> unit
  val set_pos : t -> Vector.t -> unit
  val add_pos : t -> Vector.t -> unit
  val set_force : t -> Vector.t -> unit
  val add_force : t -> Vector.t -> unit
  val create : center:Vector.t -> mass:float -> radius:float -> t
end

module Box : sig
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    ; mutable theta : float
    ; mutable velocity : Vector.t
    ; mutable net_force : Vector.t
    ; mass : float
    }

  val update_pos : t -> float -> unit
  val update_vel : t -> float -> unit
  val set_vel : t -> Vector.t -> unit
  val add_vel : t -> Vector.t -> unit
  val set_pos : t -> Vector.t -> unit
  val add_pos : t -> Vector.t -> unit
  val set_force : t -> Vector.t -> unit
  val add_force : t -> Vector.t -> unit
  val create : min:Vector.t -> max:Vector.t -> mass:float -> t
end

module Line : sig
  type t =
    { mutable first_endp : Vector.t
    ; mutable second_endp : Vector.t
    }

  val calc_slope : t -> float
  val length : t -> float
  val length_squared : t -> float
  val x_length : t -> float
  val y_length : t -> float
  val center : t -> Vector.t
  val create : first_endp:Vector.t -> second_endp:Vector.t -> t
end

module Cup : sig
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    }

  val create : min:Vector.t -> max:Vector.t -> t
end

type t =
  | Ball
  | Line
  | Cup
  | Box

val equal : t -> t -> bool

module ObjectSelector : sig
  type t =
    | Ball of Ball.t
    | Line of Line.t
    | Cup of Cup.t
    | Box of Box.t
end

val find_min_max : Vector.t -> Vector.t -> Vector.t * Vector.t
val ball_point_collide : Ball.t -> Vector.t -> bool
val box_point_collide : Box.t -> Vector.t -> bool
val cup_point_collide : Cup.t -> Vector.t -> bool
val line_point_collide : Line.t -> Vector.t -> bool
