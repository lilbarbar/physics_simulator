open! Core
open! Collisions
open! Objects
open! Vector

let resolve_ball_ball_collision (ball1 : Ball.t) (ball2 : Ball.t) =
  let ball1_center = ball1.center in
  let ball2_center = ball2.center in
  let ball1_vel = ball1.velocity in
  let ball2_vel = ball2.velocity in
  let ball1_mass = ball1.mass in
  let ball2_mass = ball2.mass in
  let ball1_radius = ball1.radius in
  let ball2_radius = ball2.radius in
  let vector_from_ball2_ball1 = Vector.( - ) ball1_center ball2_center in
  let vector_from_ball2_ball1_norm =
    Vector.normalize vector_from_ball2_ball1
  in
  let ball1_vel_dot_prod =
    Vector.dot_product vector_from_ball2_ball1_norm ball1_vel
  in
  let ball1_vel_norm_comp =
    Vector.( * ) vector_from_ball2_ball1_norm ball1_vel_dot_prod
  in
  let ball1_vel_tang_comp = Vector.( - ) ball1_vel ball1_vel_norm_comp in
  let ball2_vel_dot_prod =
    Vector.dot_product vector_from_ball2_ball1_norm ball2_vel
  in
  let ball2_vel_norm_comp =
    Vector.scale vector_from_ball2_ball1_norm ~k:ball2_vel_dot_prod
  in
  let ball2_vel_tang_comp = Vector.( - ) ball2_vel ball2_vel_norm_comp in
  let new_ball1_vel_norm =
    ((ball1_vel_dot_prod
      *. (ball1_mass -. (Constants.coeff_of_restitution *. ball2_mass)))
     +. ((1.0 +. Constants.coeff_of_restitution)
         *. ball2_mass
         *. ball2_vel_dot_prod))
    /. (ball1_mass +. ball2_mass)
  in
  let new_ball1_vel_norm_vector =
    Vector.( * ) vector_from_ball2_ball1_norm new_ball1_vel_norm
  in
  let new_ball2_vel_norm =
    ((ball2_vel_dot_prod
      *. (ball2_mass -. (Constants.coeff_of_restitution *. ball1_mass)))
     +. ((1.0 +. Constants.coeff_of_restitution)
         *. ball1_mass
         *. ball1_vel_dot_prod))
    /. (ball1_mass +. ball2_mass)
  in
  let new_ball2_vel_norm_vector =
    Vector.( * ) vector_from_ball2_ball1_norm new_ball2_vel_norm
  in
  let new_ball1_vel =
    Vector.( + ) new_ball1_vel_norm_vector ball1_vel_tang_comp
  in
  let new_ball2_vel =
    Vector.( + ) new_ball2_vel_norm_vector ball2_vel_tang_comp
  in
  let new_ball1_vel =
    if Float.( < ) (Vector.mag new_ball1_vel) Constants.velocity_threshold
    then Vector.zero ()
    else new_ball1_vel
  in
  let new_ball2_vel =
    if Float.( < ) (Vector.mag new_ball2_vel) Constants.velocity_threshold
    then Vector.zero ()
    else new_ball2_vel
  in
  ball1.velocity <- new_ball1_vel;
  ball2.velocity <- new_ball2_vel;
  let center_dist = Vector.mag vector_from_ball2_ball1 in
  let overlap_dist = ball1_radius +. ball2_radius -. center_dist in
  let total_mass = ball1_mass +. ball2_mass in
  let correction_vector =
    Vector.scale vector_from_ball2_ball1_norm ~k:(overlap_dist /. total_mass)
  in
  ball1.center
  <- Vector.( + ) ball1.center (Vector.( * ) correction_vector ball2_mass);
  ball2.center
  <- Vector.( - ) ball2.center (Vector.( * ) correction_vector ball1_mass)
;;

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
      (Float.abs (980.0 *. ball.mass *. sin_theta))
  in
  let new_force : Force.t =
    { vector = new_force_vector; name = "Normal Force" }
  in
  new_force
;;

let ball_line_force_interaction (ball : Ball.t) (line : Line.t) =
  let new_force : Force.t = generate_normal_force_helper ball line in
  if ball_and_line ball line
  then (
    match
      List.find ball.forces ~f:(fun force ->
        Force.equal new_force force && String.equal force.name "Normal Force")
    with
    | Some _ -> ()
    | None ->
      Ball.remove_force
        ball
        { vector = Vector.scale { x = 0.0; y = -980.0 } ~k:ball.mass
        ; name = "Gravity"
        };
      (match
         List.find ball.forces ~f:(fun force ->
           String.equal force.name "Normal Force")
       with
       | Some force ->
         if
           Float.( >= )
             (force.vector.y /. force.vector.x *. Line.calc_slope line)
             0.0
         then ()
         else (
           Ball.add_force ball new_force;
           let overall_net_force : Vector.t = Ball.net_force ball in
           let counter_force : Force.t =
             { vector = Vector.( * ) overall_net_force (-1.0)
             ; name = "counter"
             }
           in
           Ball.add_force ball counter_force;
           print_s [%sexp (Ball.net_force ball : Vector.t)];
           ball.velocity <- { x = 0.0; y = 0.0 })
       | None ->
         Ball.add_force ball new_force;
         Ball.set_vel ball { x = 0.0; y = 0.0 }))
  else (
    match
      List.find ball.forces ~f:(fun force -> Force.equal force new_force)
    with
    | Some force ->
      Ball.remove_force ball force;
      let gravity_vector =
        Vector.scale { x = 0.0; y = -980.0 } ~k:ball.mass
      in
      if List.is_empty ball.forces
      then Ball.add_force ball { vector = gravity_vector; name = "Gravity" }
    | None -> ())
;;

let ball_cup_force_interaction (ball : Ball.t) (cup : Cup.t) =
  if ball_in_cup ball cup
  then (
    print_string "ball in cup";
    ball.center
    <- { x = (cup.min.x +. cup.max.x) /. 2.0; y = cup.min.y +. ball.radius };
    ball.velocity <- { x = 0.0; y = 0.0 };
    ball.forces <- [])
;;

let all_ball_and_line_forces (canvas : Canvas.t) =
  let all_balls = canvas.balls in
  let all_lines = canvas.lines in
  List.iter all_balls ~f:(fun ball ->
    List.iter all_lines ~f:(fun line ->
      ball_line_force_interaction ball line))
;;

let all_ball_and_cup_forces (canvas : Canvas.t) =
  let all_balls = canvas.balls in
  let all_cups = canvas.cups in
  List.iter all_balls ~f:(fun ball ->
    List.iter all_cups ~f:(fun cup -> ball_cup_force_interaction ball cup))
;;

let handle_ball_ball_reactions (canvas : Canvas.t) =
  List.iteri canvas.balls ~f:(fun i ball1 ->
    let rest = List.drop canvas.balls Int.(i + 1) in
    List.iter rest ~f:(fun ball2 ->
      if Collisions.ball_and_ball ball1 ball2
      then resolve_ball_ball_collision ball1 ball2))
;;

let react (canvas : Canvas.t) = handle_ball_ball_reactions canvas
