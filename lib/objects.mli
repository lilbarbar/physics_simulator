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
end

module Line : sig
  type t =
    { first_endp : Vector.t
    ; second_endp : Vector.t
    }

  val calc_slope : t -> float
end

module Cup : sig
  type t =
    { min : Vector.t
    ; max : Vector.t
    }
end

type t =
  | Ball
  | Line 
  | Cup 
  | Box 

