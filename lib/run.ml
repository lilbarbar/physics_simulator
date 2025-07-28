open! Core

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
  every ~stop:world_over 0.1 ~f:(fun () ->
    World.step world;
    World_graphics.render world;
    match World.world_state world with
    | Paused | Failure -> world_over := true
    | In_progress | Clear -> ())
;;

let run () =
  let world = World_graphics.init_exn () in
  World_graphics.render world;
  let world_over = ref false in
  handle_steps world ~world_over
;;
