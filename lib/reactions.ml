open! Core
open! Collisions
open! Objects

let ball_line_force_interaction (ball : Ball.t) (line : Line.t) =
  if ball_and_line ball line
  then (
    match
      List.find ball.forces ~f:(fun force ->
        String.equal force.name "Normal Force")
    with
    | Some _ -> ()
    | None ->
      let line_vector = Vector.( - ) line.second_endp line.first_endp in
      let sin_theta = line_vector.y /. Vector.mag line_vector in
      let cos_theta = line_vector.x /. Vector.mag line_vector in
      let gravity_of_ball =
        Option.value_exn
          (List.find ball.forces ~f:(fun force ->
             String.equal force.name "Gravity"))
      in
      let normal_force_magnitude =
        Float.abs (Vector.mag gravity_of_ball.vector *. cos_theta)
      in
      let normal_force_x = normal_force_magnitude *. sin_theta in
      let normal_force_y = normal_force_magnitude *. cos_theta in
      let normal_force_vector : Vector.t =
        { x = normal_force_x; y = normal_force_y }
      in
      let normal_force : Force.t =
        { vector = normal_force_vector; name = "Normal Force" }
      in
      Ball.add_force ball normal_force)
  else (
    match
      List.find ball.forces ~f:(fun force ->
        String.equal force.name "Normal Force")
    with
    | Some force -> Ball.remove_force ball force
    | None -> ())
;;

let ball_cup_force_interaction (ball : Ball.t) (cup : Cup.t) =
  if ball_in_cup ball cup
  then
    ball.center
    <- { x = (cup.min.x +. cup.max.x) /. 2.0; y = cup.min.y +. ball.radius };
  ball.velocity <- { x = 0.0; y = 0.0 };
  ball.forces <- []
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
