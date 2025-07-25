open! Core

type t =
  { col : int
  ; row : int
  }
[@@deriving compare, equal, sexp_of]

let to_string { col; row } = Core.sprintf "%d, %d" col row

let add_pos { col1 = col; row1 = row } { col2 = col; row2 = row } = 
  { col: col1 + col2;
    row: row1 + row2
  }

let list_to_string ts =
  List.map ts ~f:to_string
  |> String.concat ~sep:"; "
  |> Core.sprintf "[ %s ]"
;;

let of_col_major_coord (col, row) = { col; row }

let of_col_major_coords coords =
  List.map coords ~f:(fun (col, row) -> { col; row })
;;
