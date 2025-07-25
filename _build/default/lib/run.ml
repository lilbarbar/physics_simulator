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

let handle_steps (game : Engine.t) ~game_over =
  every ~stop:game_over 0.1 ~f:(fun () ->
    Engine.step game;
    Engine_graphics.render game;
    match Engine.game_state game with
    | Game_over _ | Win -> game_over := true
    | In_progress -> ())
;;

let run () =
  let game = Engine_graphics.init_exn () in
  Engine_graphics.render game;
  let game_over = ref false in
  handle_steps game ~game_over
;;
