open! Core

type t =
  { x : float
  ; y : float
  }
[@@deriving compare, equal, sexp_of]

let to_string { x; y } = [%string "%{x#Float} %{y#Float}"]

let list_to_string ts =
  List.map ts ~f:to_string
  |> String.concat ~sep:"; "
  |> Core.sprintf "[ %s ]"
;;

let of_x_major_coord (x, y) = { x; y }
let of_x_major_coords coords = List.map coords ~f:(fun (x, y) -> { x; y })

let ( + ) { x = x1; y = y1 } { x = x2; y = y2 } : t =
  { x = x1 +. x2; y = y1 +. y2 }
;;

let ( - ) { x = x1; y = y1 } { x = x2; y = y2 } : t =
  { x = x1 -. x2; y = y1 -. y2 }
;;

let multi_sum (vectors : t list) : t =
  List.fold vectors ~init:{ x = 0.0; y = 0.0 } ~f:(fun acc vector ->
    acc + vector)
;;

let dist_squared { x = x1; y = y1 } { x = x2; y = y2 } : float =
  let ydiff = y2 -. y1 in
  let xdiff = x2 -. x1 in
  (ydiff *. ydiff) +. (xdiff *. xdiff)
;;

let dist v1 v2 : float = sqrt (dist_squared v1 v2)

let dot_product { x = x1; y = y1 } { x = x2; y = y2 } : float =
  (x1 *. x2) +. (y1 *. y2)
;;

let scale { x; y } ~(k : float) : t = { x = x *. k; y = y *. k }
let mag_squared { x; y } : float = (x *. x) +. (y *. y)
let mag { x; y } : float = sqrt (mag_squared { x; y })

let angle_between v1 v2 : float =
  let dot_product = dot_product v1 v2 in
  let v1_mag = mag v1 in
  let v2_mag = mag v2 in
  Float.acos (dot_product /. (v1_mag *. v2_mag))
;;

let normalize { x; y } : t =
  let mag = mag { x; y } in
  if Float.equal mag 0.0 then { x; y } else { x = x /. mag; y = y /. mag }
;;

let is_zero v1 : bool =
  let mag = mag v1 in
  Float.equal mag 0.0
;;

let rotate { x; y } ~(theta : float) : t =
  let sin_theta = Float.sin theta in
  let cos_theta = Float.cos theta in
  let rot_x = (x *. cos_theta) -. (y *. sin_theta) in
  let rot_y = (x *. sin_theta) +. (y *. cos_theta) in
  { x = rot_x; y = rot_y }
;;
