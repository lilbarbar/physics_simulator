open! Core
open! Async
open! Objects
open! Vector

let ball_point_collide (ball : Ball.t) (point : Vector.t) =
  print_endline "ball_point_collide";
  let ball_point_dist_squared = Vector.dist_squared ball.center point in
  let radius_squared = ball.radius *. ball.radius in
  Float.( <= ) ball_point_dist_squared radius_squared
;;

let box_point_collide (box : Box.t) (point : Vector.t) =
  Float.( <= ) point.x box.max.x
  && Float.( >= ) point.x box.min.x
  && Float.( <= ) point.y box.max.y
  && Float.( >= ) point.y box.min.y
;;

let cup_point_collide (cup : Cup.t) (point : Vector.t) =
  Float.( <= ) point.x cup.max.x
  && Float.( >= ) point.x cup.min.x
  && Float.( <= ) point.y cup.max.y
  && Float.( >= ) point.y cup.min.y
;;

let line_point_collide (line : Line.t) (point : Vector.t) =
  let tolerance = 1.25 in
  let dist_first_endp = Vector.dist line.first_endp point in
  let dist_second_endp = Vector.dist line.second_endp point in
  let line_length = Line.length line in
  let dist_diff =
    Float.abs (dist_first_endp +. dist_second_endp -. line_length)
  in
  Float.( <= ) dist_diff (tolerance *. tolerance *. 2.0)
;;

let ball_and_ball (ball1 : Ball.t) (ball2 : Ball.t) : bool =
  Float.( <= )
    (Vector.mag (Vector.( - ) ball1.center ball2.center))
    (ball1.radius +. ball2.radius)
;;

let ball_and_ball_collision_point (ball1 : Ball.t) (ball2 : Ball.t)
  : Vector.t
  =
  Vector.( / ) (Vector.( + ) ball1.center ball2.center) 2.0
;;

let ball_and_line (ball : Ball.t) (line : Line.t) : bool =
  let vector_A = Vector.( - ) ball.center line.first_endp in
  let vector_B = Vector.( - ) line.second_endp line.first_endp in
  let projection =
    Vector.( * )
      vector_B
      (Vector.dot_product vector_A vector_B
       /. Vector.dot_product vector_B vector_B)
  in
  let ortho_to_projection = Vector.( - ) vector_A projection in
  let min_x, max_x =
    match Float.( <= ) line.second_endp.x line.first_endp.x with
    | true -> line.second_endp.x, line.first_endp.x
    | false -> line.first_endp.x, line.second_endp.x
  in
  let min_y, max_y =
    match Float.( <= ) line.second_endp.y line.first_endp.y with
    | true -> line.second_endp.y, line.first_endp.y
    | false -> line.first_endp.y, line.second_endp.y
  in
  Float.( <= ) (Vector.mag ortho_to_projection) ball.radius
  && Float.( >= ) ball.center.x min_x
  && Float.( <= ) ball.center.x max_x
  && Float.( >= ) ball.center.y min_y
  && Float.( <= ) ball.center.y max_y
;;

let ball_and_line_collision_point (ball : Ball.t) (line : Line.t) : Vector.t =
  let vector_A = Vector.( - ) ball.center line.first_endp in
  let vector_B = Vector.( - ) line.second_endp line.first_endp in
  let projection =
    Vector.( * )
      vector_B
      (Vector.dot_product vector_A vector_B
       /. Vector.dot_product vector_B vector_B)
  in
  Vector.( + ) projection line.first_endp
;;

let ball_in_cup (ball : Ball.t) (cup : Cup.t) : bool =
  print_s [%sexp (ball.center : Vector.t)];
  print_s [%sexp (cup.min : Vector.t)];
  print_s [%sexp (cup.max : Vector.t)];
  Float.compare ball.center.x (cup.min.x +. ball.radius) >= 0
  && Float.compare ball.center.x (cup.max.x -. ball.radius) <= 0
  && Float.compare ball.center.y (cup.min.y +. ball.radius) >= 0
  && Float.compare ball.center.y (cup.max.y -. ball.radius) <= 0
;;

let ball_and_line_collision_point (ball : Ball.t) (line : Line.t) : Vector.t =
  let vector_A = Vector.( - ) ball.center line.first_endp in
  let vector_B = Vector.( - ) line.second_endp line.first_endp in
  let projection =
    Vector.( * )
      vector_B
      (Vector.dot_product vector_A vector_B
       /. Vector.dot_product vector_B vector_B)
  in
  Vector.( + ) projection line.first_endp
;;

let ball_resting_in_cup (ball : Ball.t) (cup : Cup.t) : bool =
  Float.compare ball.center.x (cup.min.x +. ball.radius) > 0
  && Float.compare ball.center.x (cup.max.x -. ball.radius) < 0
  && Float.compare ball.center.y (cup.min.y +. ball.radius) = 0
;;

let ball_collides_with_cup_wall (ball : Ball.t) (cup : Cup.t) : bool =
  (Float.compare ball.center.y (cup.min.y +. ball.radius) > 0
   && Float.compare ball.center.y (cup.max.y -. ball.radius) < 0)
  && (Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
      || Float.compare ball.center.x (cup.min.x +. ball.radius) = 0
      || Float.compare ball.center.x (cup.max.x -. ball.radius) = 0
      || Float.compare ball.center.x (cup.max.x +. ball.radius) = 0)
;;

let ball_collides_with_cup_wall_collision_point (ball : Ball.t) (cup : Cup.t)
  =
  if Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
  then { x = ball.center.x +. ball.radius; y = ball.center.y }
  else if Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
  then { x = ball.center.x -. ball.radius; y = ball.center.y }
  else if Float.compare ball.center.x (cup.min.x -. ball.radius) = 0
  then { x = ball.center.x +. ball.radius; y = ball.center.y }
  else { x = ball.center.x -. ball.radius; y = ball.center.y }
;;

let ball_collides_with_cup_bottom (ball : Ball.t) (cup : Cup.t) : bool =
  (Float.compare ball.center.x (cup.min.x +. ball.radius) >= 0
   && Float.compare ball.center.x (cup.max.x -. ball.radius) <= 0)
  && Float.compare ball.center.y (cup.min.y -. ball.radius) = 0
;;

let ball_collides_with_cup_bottom_collision_point
      (ball : Ball.t)
      (cup : Cup.t)
  : Vector.t
  =
  { x = ball.center.x; y = ball.center.y +. ball.radius }
;;

let ball_collides_with_box (ball : Ball.t) (box : Box.t) : bool =
  let left_side =
    Float.compare (ball.center.x +. ball.radius) box.min.x = 0
    && Float.compare ball.center.y box.min.y > 0
    && Float.compare ball.center.y box.max.y < 0
  in
  let top_side =
    Float.compare (ball.center.y -. ball.radius) box.max.y = 0
    && Float.compare ball.center.x box.min.x > 0
    && Float.compare ball.center.x box.max.x < 0
  in
  let right_side =
    Float.compare (ball.center.x -. ball.radius) box.min.x = 0
    && Float.compare ball.center.y box.min.y > 0
    && Float.compare ball.center.y box.max.y < 0
  in
  let bottom_side =
    Float.compare (ball.center.y +. ball.radius) box.min.y = 0
    && Float.compare ball.center.x box.min.x > 0
    && Float.compare ball.center.x box.max.x < 0
  in
  let top_left_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.min.x; y = box.max.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let top_right_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.max.x; y = box.max.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let bottom_right_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.max.x; y = box.min.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let bottom_left_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.min.x; y = box.min.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  left_side
  || top_side
  || right_side
  || bottom_side
  || top_left_corner
  || top_right_corner
  || bottom_right_corner
  || bottom_left_corner
;;

let ball_collides_with_box_collision_point (ball : Ball.t) (box : Box.t)
  : Vector.t
  =
  let left_side =
    Float.compare (ball.center.x +. ball.radius) box.min.x = 0
    && Float.compare ball.center.y box.min.y > 0
    && Float.compare ball.center.y box.max.y < 0
  in
  let top_side =
    Float.compare (ball.center.y -. ball.radius) box.max.y = 0
    && Float.compare ball.center.x box.min.x > 0
    && Float.compare ball.center.x box.max.x < 0
  in
  let right_side =
    Float.compare (ball.center.x -. ball.radius) box.min.x = 0
    && Float.compare ball.center.y box.min.y > 0
    && Float.compare ball.center.y box.max.y < 0
  in
  let bottom_side =
    Float.compare (ball.center.y +. ball.radius) box.min.y = 0
    && Float.compare ball.center.x box.min.x > 0
    && Float.compare ball.center.x box.max.x < 0
  in
  let top_left_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.min.x; y = box.max.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let top_right_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.max.x; y = box.max.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let bottom_right_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.max.x; y = box.min.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  let bottom_left_corner =
    let vec1 = ball.center in
    let vec2 = { x = box.min.x; y = box.min.y } in
    Float.compare (Vector.dist vec1 vec2) ball.radius = 0
  in
  ignore bottom_left_corner;
  if left_side
  then { x = box.min.x; y = ball.center.y }
  else if top_side
  then { x = ball.center.x; y = box.max.y }
  else if right_side
  then { x = box.max.x; y = ball.center.y }
  else if bottom_side
  then { x = ball.center.x; y = box.min.y }
  else if top_left_corner
  then { x = box.min.x; y = box.max.y }
  else if top_right_corner
  then { x = box.max.x; y = box.max.y }
  else if bottom_right_corner
  then { x = box.max.x; y = box.min.y }
  else { x = box.min.x; y = box.min.y }
;;
