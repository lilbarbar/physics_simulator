open! Core

type t =
  { height : int
  ; width : int
  }
[@@deriving sexp_of]

let create ~height ~width = { height; width }
let create_unlabeled height width = { height; width }

let in_bounds t { Position.row; col } =
  
  if row < t.height && row >= 0 && col < t.width && col >= 0 then true else false
;;

let all_positions t =
  List.concat_map (List.range 0 t.height) ~f:(fun row ->
    List.map (List.range 0 t.width) ~f:(fun col -> { Position.row; col }))
;;