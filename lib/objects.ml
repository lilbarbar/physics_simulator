open! Core

module Ball = struct
  type t =
    { x_pos : int
    ; y_pos : int
    ; x_vel : int
    ; y_vel : int
    ; mass : int
    ; forces : Force.t list
    }

  let net_force_x t =
    List.fold t.forces ~init:0.0 ~f:(fun init force ->
      init +. force.x_direction)
  ;;

  let net_force_y t =
    List.fold t.forces ~init:0.0 ~f:(fun init force ->
      init +. force.y_direction)
  ;;

  let acceleration_x t = net_force_x t /. float_of_int t.mass
  let acceleration_y t = net_force_y t /. float_of_int t.mass
  let next_x t = t.x_pos + t.x_vel
  let next_y t = t.y_pos + t.y_vel
  let next_v_x t = int_of_float (float_of_int t.x_vel +. acceleration_x t)
  let next_v_y t = int_of_float (float_of_int t.y_vel +. acceleration_y t)
end

module Line = struct
  type t =
    { x1_pos : int
    ; y1_pos : int
    ; x2_pos : int
    ; y2_pos : int
    ; friction_constant : float
    }

  let calc_slope t =
    int_of_float
      (float_of_int (t.y2_pos - t.y1_pos)
       /. float_of_int (t.x2_pos - t.x1_pos))
  ;;
end



module Cup = struct
  type t =
    { x_pos : int
    ; y_pos : int
    }
end
