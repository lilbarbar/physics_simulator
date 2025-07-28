open! Core
open! Vectors

module Ball = struct
  type t =
    { center : Position.t
    ; velocity : Velocity.t
    ; mass : float
    ; radius : int
    }
  (*
     let next_x t = t.x_pos + t.x_vel
     let next_y t = t.y_pos + t.y_vel
     let next_v_x t = int_of_float (float_of_int t.x_vel +. acceleration_x t)
     let next_v_y t = int_of_float (float_of_int t.y_vel +. acceleration_y t) *)
end

module Box = struct
  type t =
    { min : Position.t
    ; max : Position.t
    ; velocity : Velocity.t
    ; mass : Velocity.t
    }
end

module Line = struct
  type t =
    { first_endp : Position.t
    ; second_endp : Position.t
    }

  let calc_slope t =
    (t.second_endp.y -. t.first_endp.y) /. (t.second_endp.x -. t.first_endp.x)
  ;;
end

module Cup = struct
  type t =
    { x_pos : int
    ; y_pos : int
    }
end


module MovableObject = struct
  type t =
    | Ball
    | Box
end

let next_position = (t : MovableObject.t) (dt : float) = 
  let increment_by_x = dt * t.Velocity
  
