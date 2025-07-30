open! Core
open! Async
open! Graphics

let every seconds ~f ~stop =
  let open Async in
  let rec loop () =
    if !stop
    then return ()
    else
      Clock.after (Time_float.Span.of_sec seconds)
      >>= fun () ->
      f ();
      loop ()
  in
  don't_wait_for (loop ())
;;

let handle_steps (world : World.t) ~world_over =
  every ~stop:world_over 1.0 ~f:(fun () ->
    World.step world;
    print_endline "stepping";
    World_graphics.render world;
    match world.world_state with
    | Paused | Failure -> world_over := true
    | In_progress | Clear -> ())
;;

let run () =
  let world = World.create () in
  World_graphics.init_exn world.ui;
  World_graphics.render world;
  let world_over = ref false in
  handle_steps world ~world_over
;;
