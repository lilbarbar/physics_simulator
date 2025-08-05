open! Core
open! Collisions
open! Objects
open! Vector

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
      List.find ball.forces ~f:(fun force -> Force.equal new_force force)
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
           String.equal force.name "String")
       with
       | Some force ->
         if
           Float.( >= )
             (force.vector.y /. force.vector.x *. Line.calc_slope line)
             0.0
         then ()
         else ball.forces <- [];
         ball.velocity <- { x = 0.0; y = 0.0 }
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

let update_forces (canvas : Canvas.t) =
  all_ball_and_line_forces canvas;
  all_ball_and_cup_forces canvas
;;
