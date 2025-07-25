open! Core
open! Async
open! Physics_simulator
open! World_graphics
open! World
open! Objects

let do_a_thing (n : int) : unit Deferred.t =
  let%bind () = Clock_ns.after (Time_ns_unix.Span.of_int_sec n) in
  Core.printf "Finished waiting %i seconds\n%!" n;
  return ()
;;

let main () =
  print_endline "Hello, World!";
  (* let test_func () =  Graphics.open_graph ("") in *)
  Graphics.open_graph " 20000 x 20000 ";
  Graphics.resize_window 750 500;
  let black = Graphics.rgb 000 000 000 in
  let gray = Graphics.rgb 128 128 128 in
  (* let green = Graphics.rgb 000 255 000 in
     let head_color = Graphics.rgb 100 100 125 in
     let red = Graphics.rgb 255 000 000 in
     let gold = Graphics.rgb 255 223 0 in *)
  let white = Graphics.rgb 255 255 255 in
  Graphics.set_color black;
  Graphics.fill_rect 0 0 750 500;
  Graphics.set_color gray;
  Graphics.fill_rect 500 0 250 500;
  Graphics.set_color white;
  Graphics.fill_rect 700 650 100 50;
  Graphics.fill_rect 800 650 100 50;
  Graphics.fill_rect 900 650 100 50;
  Graphics.set_color black;
  (* let ball_text = "Ball" in
  let line_text = "Line" in
  let cup_text = "Cup" in
  Graphics.moveto 1095 920;
  Graphics.draw_string ball_text;
  Graphics.moveto 1240 920;
  Graphics.draw_string line_text;
  Graphics.moveto 1395 920;
  Graphics.draw_string cup_text;
  Graphics.set_color white;
  Graphics.moveto 0 0;
  Graphics.draw_string "Current Object: Ball"; *)
  (* (let%bind () = Clock_ns.after (Time_ns_unix.Span.of_int_sec 10))  in *)
  let%bind () = do_a_thing 1000 in
  Graphics.close_graph ();
  return ()
;;

let command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return () in
     fun () -> main ())
;;

let () = Command_unix.run command
