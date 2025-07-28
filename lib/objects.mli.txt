open! Core

module Ball : sig
  type t =
    { x_pos : int
    ; y_pos : int
    ; x_vel : int
    ; y_vel : int
    ; mass : int
    ; forces : Force.t list
    }
end

module Line : sig
  type t =
    { x1_pos : int
    ; y1_pos : int
    ; x2_pos : int
    ; y2_pos : int
    ; friction_constant : float
    }

  val calc_slope : t -> int
end

module Cup : sig
  type t =
    { x_pos : int
    ; y_pos : int
    }
end
