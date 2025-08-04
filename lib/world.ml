open! Core
open! Objects
open! Async
open! Graphics

type t =
  { mutable world_state : World_state.t
  ; mutable ui : Interface.t
  ; mutable click_state : Click_state.t
  }

let create () =
  { world_state = World_state.Paused
  ; ui = Interface.create ~height:500 ~width:750
  ; click_state = Click_state.Free_state
  }
;;
