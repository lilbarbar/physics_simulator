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

let handle_steps (game : Engine.t) ~engine_over =
  every ~stop:engine_over 0.1 ~f:(fun () ->
    Engine.step game;
    Engine_graphics.render game;
    match Engine.engine_state game with
    | Paused | Failure -> engine_over := true
    | In_progress | Clear -> ())
;;

let run () =
  let game = Engine_graphics.init_exn () in
  Engine_graphics.render game;
  let engine_over = ref false in
  handle_steps game ~engine_over
;;
