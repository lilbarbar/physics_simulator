open Vectors

module Ball : sig
  type t =
    { mutable center : Position.t
    ; mutable velocity : Velocity.t
    ; mutable net_force : Force.t
    ; mass : float
    ; radius : float
    } [@@deriving sexp]

  val update_pos : t -> float -> unit
  val update_vel : t -> float -> unit

  val set_vel : t -> Velocity.t -> unit
  val add_vel : t -> Velocity.t -> unit

  val set_pos : t -> Position.t -> unit
  val add_pos : t -> Position.t -> unit

  val set_force : t -> Force.t -> unit
  val add_force : t -> Force.t -> unit
end

module Box : sig
  type t =
    { mutable min : Position.t
    ; mutable max : Position.t
    ; mutable theta : float
    ; mutable velocity : Velocity.t
    ; mutable net_force : Force.t
    ; mass : float
    } [@@deriving sexp]

  val update_pos : t -> float -> unit
  val update_vel : t -> float -> unit

  val set_vel : t -> Velocity.t -> unit
  val add_vel : t -> Velocity.t -> unit

  val set_pos : t -> Position.t -> unit
  val add_pos : t -> Position.t -> unit

  val set_force : t -> Force.t -> unit
  val add_force : t -> Force.t -> unit
end

module Line : sig
  type t =
    { first_endp : Position.t
    ; second_endp : Position.t
    } [@@deriving sexp]

  val calc_slope : t -> float
end

module Cup : sig
  type t =
    { min : Position.t
    ; max : Position.t
    }
end
