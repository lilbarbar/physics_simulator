open! Core

type t =
  { height : int
  ; width : int
  }
[@@deriving sexp_of]

let create ~height ~width = { height; width }
let create_unlabeled height width = { height; width }

(* 
let in_bounds t { x; y } =
  if row < t.height && row >= 0 && col < t.width && col >= 0 then true else false
;; *)