open! Core

module Ball = struct
  type t =
    { mutable center : Vector.t
    ; mutable velocity : Vector.t
    ; mutable net_force : Vector.t
    ; mass : float
    ; radius : float
    }

  let update_pos t (dt : float) =
    let dx = Vector.( * ) t.velocity dt in
    let new_position = Vector.( + ) t.center dx in
    t.center <- new_position
  ;;

  let update_vel t (dt : float) =
    let acceleration = Vector.( / ) t.net_force t.mass in
    let dv = Vector.( * ) acceleration dt in
    let new_velocity = Vector.( + ) t.velocity dv in
    t.velocity <- new_velocity
  ;;

  let set_vel t (nv : Vector.t) = t.velocity <- nv
  let add_vel t (dv : Vector.t) = t.velocity <- Vector.( + ) t.velocity dv
  let set_pos t (np : Vector.t) = t.center <- np
  let add_pos t (dx : Vector.t) = t.center <- Vector.( + ) t.center dx
  let set_force t (nf : Vector.t) = t.net_force <- nf

  let add_force t (df : Vector.t) =
    t.net_force <- Vector.( + ) t.net_force df
  ;;
end

module Box = struct
  type t =
    { mutable min : Vector.t
    ; mutable max : Vector.t
    ; mutable theta : float
    ; mutable velocity : Vector.t
    ; mutable net_force : Vector.t
    ; mass : float
    } 

  let update_pos t (dt : float) =
    let dx = Vector.( * ) t.velocity dt in
    let new_min_pos = Vector.( + ) t.min dx in
    let new_max_pos = Vector.( + ) t.max dx in
    t.min <- new_min_pos;
    t.max <- new_max_pos
  ;;

  let update_vel t (dt : float) =
    let acceleration = Vector.( / ) t.net_force t.mass in
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

  let set_force t (nf : Vector.t) = t.net_force <- nf

  let add_force t (df : Vector.t) =
    t.net_force <- Vector.( + ) t.net_force df
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
end

module Cup = struct
  type t =
    { min : Vector.t
    ; max : Vector.t
    }
end
