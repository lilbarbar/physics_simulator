open! Core
open! Objects

type t =
  { height : int
  ; width : int
  ; mutable balls : Ball.t list
  ; mutable lines : Line.t list
  ; mutable cups : Cup.t list
  ; mutable boxes : Box.t list
  }

let create ~height ~width =
  { height; width; balls = []; lines = []; cups = []; boxes = [] }
;;

let in_bounds t x y = x >= 0 && x < t.width && y >= 0 && y < t.height

let bound_objects t =
  List.iter t.balls ~f:(fun ball ->
    let radius = ball.radius in
    let clamped_x =
      Float.min
        (Float.max ball.center.x radius)
        (Float.of_int t.width -. radius)
    in
    let clamped_y =
      Float.min
        (Float.max ball.center.y radius)
        (Float.of_int t.height -. radius)
    in
    ball.center <- { x = clamped_x; y = clamped_y })
;;

let add_ball t obj = t.balls <- t.balls @ [ obj ]
let add_line t obj = t.lines <- t.lines @ [ obj ]
let add_cup t obj = t.cups <- t.cups @ [ obj ]
let add_box t obj = t.boxes <- t.boxes @ [ obj ]

let clear t =
  t.balls <- [];
  t.cups <- [];
  t.lines <- [];
  t.boxes <- []
;;
