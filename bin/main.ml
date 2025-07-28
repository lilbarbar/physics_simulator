open! Core
open! Async
open! Physics_simulator
open! Graphics
open! World_graphics
open! World
open! Objects

let create_screen () =
  print_endline "Hello, World!";
  (* let test_func () =  Graphics.open_graph ("") in *)
  Graphics.open_graph " 20000 x 20000 ";
  Graphics.resize_window 1500 1000;
  let black = Graphics.rgb 000 000 000 in
  let gray = Graphics.rgb 128 128 128 in
  (* let green = Graphics.rgb 000 255 000 in
     let head_color = Graphics.rgb 100 100 125 in
     let red = Graphics.rgb 255 000 000 in
     let gold = Graphics.rgb 255 223 0 in *)
  let white = Graphics.rgb 255 255 255 in
  let red = Graphics.rgb 255 0 0 in
  Graphics.set_color black;
  Graphics.fill_rect 0 0 1500 1000;
  Graphics.set_color gray;
  Graphics.fill_rect 1000 0 500 1000;
  Graphics.set_color white;
  Graphics.fill_rect 1050 900 100 50;
  Graphics.fill_rect 1200 900 100 50;
  Graphics.fill_rect 1350 900 100 50;
  Graphics.set_color black;
  let ball_text = "Ball" in
  let line_text = "Line" in
  let cup_text = "Cup" in
  Graphics.moveto 1095 920;
  Graphics.draw_string ball_text;
  Graphics.moveto 1240 920;
  Graphics.draw_string line_text;
  Graphics.moveto 1395 920;
  Graphics.draw_string cup_text;
  Graphics.set_color white;
  Graphics.moveto 0 1000;
  Graphics.draw_string "Current Object: Ball";
  Graphics.set_color red;
  Graphics.fill_rect 1050 700 400 50;
  Graphics.moveto 1225 720;
  Graphics.set_color white;
  Graphics.draw_string "CLEAR!"
;;

let rec handle_clicks () : unit =
  let event = wait_next_event [ Button_down ] in
  (* Wait for a mouse click *)
  if event.button
  then (* Check if a button was pressed (i.e., a click occurred) *)
    (
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
    (* Example: If click is within a specific region, draw a circle *)
    if click_x >= 0 && click_x < 1000 && click_y > 0 && click_y < 1000
    then (
      set_color blue;
      fill_circle click_x click_y 20;
      print_endline "Ball Button Pressed";
      handle_clicks ())
    else if
      click_x >= 1050
      && click_x < 1050 + 400
      && click_y > 700
      && click_y < 700 + 50
    then (
      create_screen ();
      handle_clicks ())
    else handle_clicks ())
  (* Continue handling clicks *)
  else handle_clicks () (* If no button down, continue waiting for clicks *)
;;

let do_a_thing (n : int) : unit Deferred.t =
  let%bind () = Clock_ns.after (Time_ns_unix.Span.of_int_sec n) in
  Core.printf "Finished waiting %i seconds\n%!" n;
  return ()
;;

let main () =
  print_endline "Hello, World!";
  (* let test_func () =  Graphics.open_graph ("") in *)
  Graphics.open_graph " 20000 x 20000 ";
  Graphics.resize_window 1500 1000;
  let black = Graphics.rgb 000 000 000 in
  let gray = Graphics.rgb 128 128 128 in
  (* let green = Graphics.rgb 000 255 000 in
     let head_color = Graphics.rgb 100 100 125 in
     let red = Graphics.rgb 255 000 000 in
     let gold = Graphics.rgb 255 223 0 in *)
  let white = Graphics.rgb 255 255 255 in
  let red = Graphics.rgb 255 0 0 in
  Graphics.set_color black;
  Graphics.fill_rect 0 0 1500 1000;
  Graphics.set_color gray;
  Graphics.fill_rect 1000 0 500 1000;
  Graphics.set_color white;
  Graphics.fill_rect 1050 900 100 50;
  Graphics.fill_rect 1200 900 100 50;
  Graphics.fill_rect 1350 900 100 50;
  Graphics.set_color black;
  let ball_text = "Ball" in
  let line_text = "Line" in
  let cup_text = "Cup" in
  Graphics.moveto 1095 920;
  Graphics.draw_string ball_text;
  Graphics.moveto 1240 920;
  Graphics.draw_string line_text;
  Graphics.moveto 1395 920;
  Graphics.draw_string cup_text;
  Graphics.set_color white;
  Graphics.moveto 0 1000;
  Graphics.draw_string "Current Object: Ball";
  Graphics.set_color red;
  Graphics.fill_rect 1050 700 400 50;
  Graphics.moveto 1225 720;
  Graphics.set_color white;
  Graphics.draw_string "CLEAR!";
  (* (let%bind () = Clock_ns.after (Time_ns_unix.Span.of_int_sec 10))  in *)
  handle_clicks ();
  let%bind () = do_a_thing 1000 in
  Graphics.close_graph ();
  return ()
;;

let rec infinite_loop () : unit =
  if true then infinite_loop () else infinite_loop ()
;;

let main2 () =
  World_graphics.create_enviornment ();
  infinite_loop ();
  return ()
;;

let command =
  Command.async
    ~summary:""
    (let%map_open.Command () = return () in
     fun () ->
       ignore main;
       main2 ())
;;

let () = Command_unix.run command
