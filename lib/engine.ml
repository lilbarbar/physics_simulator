open! Core

type t =
  { mutable engine_state : Engine_state.t
  ; canvas : Canvas.t
  }
[@@deriving sexp_of]

let to_string { engine_state; canvas } =
  Core.sprintf
    !{| %{sexp:Engine_state.t}
%{sexp:Canvas.t}
%s |}
    engine_state
    canvas
;;

let create ~height ~width ~initial_snake_length =
  let canvas = Canvas.create ~height ~width in
  { engine_state = In_progress; canvas }
;;

let engine_state t = t.engine_state

let step t =
  ignore t;
  ()
;;
