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
    match World.world_state world with
    | Paused | Failure -> world_over := true
    | In_progress | Clear -> ())
;;

let rec handle_clicks () : unit =
  let event = wait_next_event [ Button_down ] in
  if event.button
  then
    (
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
    if click_x >= 0 && click_x < 1000 && click_y > 0 && click_y < 1000
    then (
      set_color blue;
      fill_circle click_x click_y 20;
      handle_clicks ())
    else if
      click_x >= 1050
      && click_x < 1050 + 400
      && click_y > 700
      && click_y < 700 + 50
    then (
      handle_clicks ())
    else handle_clicks ())
  else handle_clicks ()
;;

(* let handle_key t key =
  let dir = Direction.of_key key in
  match dir with None -> () | Some dir -> Snake.set_direction t.snake dir
;; *)

(* let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;; *)


(* let handle_keys (game : Game.t) ~game_over =
  every ~stop:game_over 0.001 ~f:(fun () ->
    match Snake_graphics.read_key () with
    | None -> ()
    | Some key ->
      Game.handle_key game key;
      Snake_graphics.render game)
;; *)


let run () =
  let world = World.create () in
  World_graphics.init_exn ();
  World_graphics.render world;
  let world_over = ref false in
  handle_steps world ~world_over;
  handle_clicks ()
;;
