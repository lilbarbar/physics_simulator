open! Core
open! Objects
open! Async

type t =
  { mutable world_state : World_state.t
  ; mutable ui : Interface.UI.t
  ; mutable click_state : Click_state.t
  }

let create () =
  { world_state = In_progress
  ; ui = Interface.UI.create ~height:500 ~width:750
  ; click_state = Click_state.Free_state
  }
;;

let step t = ignore t
