open! Core
open! Collisions
open! Interface
open! Objects
open! Vector
open! Gravity

(* let vector_components (vector : Vector.t) (sin_theta : Float.t) : (Vector.t * Vector.t )= *)

let generate_normal_force_helper (ball : Ball.t) (line : Line.t) =
  let line_vector =
    match Float.( >= ) line.second_endp.y line.first_endp.y with
    | false -> Vector.( - ) line.second_endp line.first_endp
    | true -> Vector.( - ) line.first_endp line.second_endp
  in
  let sin_theta = line_vector.y /. Vector.mag line_vector in
  let new_force_vector =
    Vector.( * )
      (Vector.normalize line_vector)
      (Float.abs (gravity_acceleration *. ball.mass *. sin_theta))
  in
  let new_force : Force.t =
    { vector = new_force_vector; name = "Normal Force" }
  in
  new_force
;;

let generate_initial_line_velocity (ball : Ball.t) (line : Line.t) =
  let line_vector =
    match Float.( >= ) line.second_endp.y line.first_endp.y with
    | false -> Vector.( - ) line.second_endp line.first_endp
    | true -> Vector.( - ) line.first_endp line.second_endp
  in
  let unit_line_vector = Vector.normalize line_vector in
  let sin_theta = unit_line_vector.y in
  (* double check if x or y *)
  let magnitude_of_horiz_comp =
    Vector.mag ball.velocity *. Float.abs sin_theta
  in
  (* print_s
     [%sexp (Vector.( * ) unit_line_vector magnitude_of_horiz_comp : Vector.t)]; *)
  Vector.( * ) unit_line_vector magnitude_of_horiz_comp
;;

let ball_line_force_interaction_2 (ball : Ball.t) (line : Line.t) =
  if ball_and_line ball line
  then (
    print_string "touching line";
    let line_vector =
      match Float.( >= ) line.second_endp.x line.first_endp.x with
      | true -> Vector.( - ) line.second_endp line.first_endp
      | false -> Vector.( - ) line.first_endp line.second_endp
    in
    (* let line_vector = Vector.( - ) line.second_endp line.first_endp in *)
    let sin_theta = line_vector.y /. Vector.mag line_vector in
    let cos_theta = line_vector.x /. Vector.mag line_vector in
    let original_ball_velocity = ball.velocity in
    print_s [%sexp (original_ball_velocity : Vector.t)];
    let v_parallel =
      (original_ball_velocity.x *. cos_theta)
      +. (original_ball_velocity.y *. sin_theta)
    in
    let v_perp =
      -1.0
      *. ((original_ball_velocity.x *. -1.0 *. sin_theta)
          +. (original_ball_velocity.y *. cos_theta))
    in
    let new_v_x =
      (v_parallel *. cos_theta) +. (-1.0 *. sin_theta *. v_perp)
    in
    let new_v_y = (v_parallel *. sin_theta) +. (cos_theta *. v_perp) in
    let new_velocity = { x = new_v_x; y = new_v_y } in
    print_s [%sexp (new_velocity : Vector.t)];
    ball.velocity <- new_velocity)
  else ()
;;

let ball_line_force_interaction (ball : Ball.t) (line : Line.t) =
  let new_force : Force.t = generate_normal_force_helper ball line in
  if ball_and_line ball line
  then (
    (* print_s [%sexp (line : Line.t)]; *)
    match
      List.find ball.forces ~f:(fun force -> Force.equal new_force force)
    with
    | Some _ -> ()
    | None ->
      Ball.remove_force
        ball
        { vector =
            Vector.scale { x = 0.0; y = gravity_acceleration } ~k:ball.mass
        ; name = "Gravity"
        };
      (match
         List.find ball.forces ~f:(fun force ->
           String.equal force.name "Normal Force")
       with
       | None ->
         Ball.add_force ball new_force;
         Ball.set_vel ball (generate_initial_line_velocity ball line)
       | Some force ->
         (* *)
         if
           Float.( >= )
             (force.vector.y /. force.vector.x *. Line.calc_slope line)
             0.0
         then (
           match
             Float.( <= )
               (Float.abs (force.vector.y /. force.vector.x))
               (Float.abs (new_force.vector.y /. new_force.vector.x))
           with
           | true ->
             ball.forces <- [];
             ball.velocity <- { x = 0.0; y = 0.0 };
             Ball.add_force ball force
           | false ->
             ball.forces <- [];
             ball.velocity <- { x = 0.0; y = 0.0 };
             Ball.add_force ball new_force)
         else (
           Ball.add_force ball new_force;
           let overall_net_force : Vector.t = Ball.net_force ball in
           let counter_force : Force.t =
             { vector = Vector.( * ) overall_net_force (-1.0)
             ; name = "Counter"
             }
           in
           Ball.add_force ball counter_force;
           (* print_s [%sexp (Ball.net_force ball : Vector.t)]; *)
           ball.velocity <- { x = 0.0; y = 0.0 })))
  else (
    (match
       List.find ball.forces ~f:(fun force ->
         String.equal "Counter" force.name)
     with
     | Some force -> Ball.remove_force ball force
     | None -> ());
    match
      List.find ball.forces ~f:(fun force -> Force.equal force new_force)
    with
    | Some force ->
      Ball.remove_force ball force;
      let gravity_vector =
        Vector.scale { x = 0.0; y = gravity_acceleration } ~k:ball.mass
      in
      if List.is_empty ball.forces
      then Ball.add_force ball { vector = gravity_vector; name = "Gravity" }
    | None -> ())
;;

let ball_cup_force_interaction (ball : Ball.t) (cup : Cup.t) =
  if ball_in_cup ball cup
  then (
    (* print_string "ball in cup"; *)
    ball.center
    <- { x = (cup.min.x +. cup.max.x) /. 2.0; y = cup.min.y +. ball.radius };
    ball.velocity <- { x = 0.0; y = 0.0 };
    ball.forces <- [])
  else if List.is_empty ball.forces
  then (
    let gravity_vector =
      { x = 0.0; y = gravity_acceleration *. ball.mass }
    in
    Ball.add_force ball { vector = gravity_vector; name = "Gravity" })
  else ()
;;

let all_ball_and_line_forces (canvas : Canvas.t) =
  let all_balls = canvas.balls in
  let all_lines = canvas.lines in
  List.iter all_balls ~f:(fun ball ->
    List.iter all_lines ~f:(fun line ->
      ball_line_force_interaction_2 ball line))
;;

let all_ball_and_cup_forces (canvas : Canvas.t) =
  let all_balls = canvas.balls in
  let all_cups = canvas.cups in
  List.iter all_balls ~f:(fun ball ->
    List.iter all_cups ~f:(fun cup -> ball_cup_force_interaction ball cup))
;;

let update_forces (canvas : Canvas.t) =
  all_ball_and_line_forces canvas;
  all_ball_and_cup_forces canvas
;;
