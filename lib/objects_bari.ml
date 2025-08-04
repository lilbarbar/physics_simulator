open! Core
open! Async
open! Force
open! Vector

module Ball = struct
  type t =
    { mutable center : Vector.t
    ; mutable velocity : Vector.t
    ; mass : float
    ; radius : float
    ; mutable forces : Force.t list
    }
  [@@deriving equal, sexp_of]

  let update_pos t (dt : float) =
    let dx = Vector.( * ) t.velocity dt in
    let new_position = Vector.( + ) t.center dx in
    t.center <- new_position
  ;;

  let net_force t =
    List.fold t.forces ~init:{ x = 0.0; y = 0.0 } ~f:(fun init force ->
      Vector.( + ) init force.vector)
  ;;

  let update_vel t (dt : float) =
    let acceleration = Vector.( / ) (net_force t) t.mass in
    (* print_string (string_of_float acceleration.x); *)
    let dv = Vector.( * ) acceleration dt in
    let new_velocity = Vector.( + ) t.velocity dv in
    t.velocity <- new_velocity
  ;;

  let set_vel t (nv : Vector.t) = t.velocity <- nv
  let add_vel t (dv : Vector.t) = t.velocity <- Vector.( + ) t.velocity dv
  let set_pos t (np : Vector.t) = t.center <- np
  let add_pos t (dx : Vector.t) = t.center <- Vector.( + ) t.center dx
  let add_force t (force : Force.t) = t.forces <- [ force ] @ t.forces

  let remove_force t (force : Force.t) =
    t.forces
    <- List.filter t.forces ~f:(fun other_force ->
         not (Force.equal force other_force))
  ;;

  let create ~center ~mass ~radius =
    let gravity_vector : Vector.t =
      Vector.( * ) { x = 0.0; y = -9.8 } mass
    in
    { center
    ; mass
    ; radius
    ; velocity = Vector.zero ()
    ; forces = [ { vector = gravity_vector; name = "Gravity" } ]
    }
  ;;
end

module Box = struct
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    ; mutable theta : float
    ; mutable velocity : Vector.t
    ; mutable forces : Force.t list
    ; mass : float
    }

  let net_force t =
    List.fold t.forces ~init:{ x = 0.0; y = 0.0 } ~f:(fun init force ->
      Vector.( + ) init force.vector)
  ;;

  let update_pos t (dt : float) =
    let dx = Vector.( * ) t.velocity dt in
    let new_min_pos = Vector.( + ) t.min dx in
    let new_max_pos = Vector.( + ) t.max dx in
    t.min <- new_min_pos;
    t.max <- new_max_pos
  ;;

  let update_vel t (dt : float) =
    let acceleration = Vector.( / ) (net_force t) t.mass in
    let dv = Vector.( * ) acceleration dt in
    let new_velocity = Vector.( + ) t.velocity dv in
    t.velocity <- new_velocity
  ;;

  let set_vel t (nv : Vector.t) = t.velocity <- nv
  let add_vel t (dv : Vector.t) = t.velocity <- Vector.( + ) t.velocity dv

  let set_pos t (np : Vector.t) =
    t.min <- np;
    t.max <- np
  ;;

  let add_pos t (dx : Vector.t) =
    t.min <- Vector.( + ) t.min dx;
    t.max <- Vector.( + ) t.max dx
  ;;

  let add_force t (force : Force.t) = t.forces <- [ force ] @ t.forces

  let remove_force t (force : Force.t) =
    t.forces
    <- List.filter t.forces ~f:(fun other_force ->
         not (Force.equal force other_force))
  ;;

  let create ~min ~max ~mass =
    { min; max; mass; theta = 0.0; velocity = Vector.zero (); forces = [] }
  ;;
end

module Line = struct
  type t =
    { first_endp : Vector.t
    ; second_endp : Vector.t
    }

  let calc_slope t =
    let ydiff = t.second_endp.y -. t.first_endp.y in
    let xdiff = t.second_endp.x -. t.first_endp.x in
    ydiff /. xdiff
  ;;

  let create ~first_endp ~second_endp = { first_endp; second_endp }
end

module Cup = struct
  type t =
    { min : Vector.t
    ; max : Vector.t
    }

  let create ~min ~max = { min; max }
end

type t =
  | Ball
  | Line
  | Cup
  | Box
