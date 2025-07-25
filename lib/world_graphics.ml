open! Core
open! Objects
open! Graphics

let draw_ball (ball : Ball.t) =
  Graphics.fill_circle ball.x_pos ball.y_pos 50;
  print_string "Done!"
;;

let draw_line (line : Line.t) =
  Graphics.moveto line.x1_pos line.y1_pos;
  Graphics.lineto line.x2_pos line.y2_pos;
  print_string "Done!"
;;

let draw_cup (cup : Cup.t) =
  Graphics.moveto cup.x_pos cup.y_pos;
  Graphics.lineto cup.x_pos (cup.y_pos - 100);
  Graphics.moveto cup.x_pos (cup.y_pos - 100);
  Graphics.lineto (cup.x_pos + 75) (cup.y_pos - 100);
  Graphics.moveto (cup.x_pos + 75) (cup.y_pos - 100);
  Graphics.lineto (cup.x_pos + 75) cup.y_pos;
  print_string "Done!"
;;

let generate_button
      text
      ~x_pos
      ~y_pos
      ~width
      ~height
      ~color_button
      ~color_text
  =
  Graphics.set_color color_button;
  Graphics.fill_rect x_pos y_pos width height;
  (* Graphics.set_font "-*-courier-medium-r-normal--*-*-*-*-*-*-iso8859-1"; *)
  (* Graphics.set_text_size 12 *)
  let text_width, text_height = Graphics.text_size text in
  Graphics.moveto
    (x_pos + (width / 2) - (text_width / 2))
    (y_pos - (height / 2) - (text_height / 2));
  Graphics.set_color color_text;
  Graphics.draw_string text
;;

let create_enviornment ?(env_width = 750) ?(env_height = 500) () =
  Graphics.open_graph "first graph";
  Graphics.resize_window env_width env_height;
  let black = Graphics.rgb 000 000 000 in
  let gray = Graphics.rgb 128 128 128 in
  let white = Graphics.rgb 255 255 255 in
  Graphics.set_color black;
  Graphics.fill_rect 0 0 env_width env_height;
  Graphics.set_color gray;
  Graphics.fill_rect (2 * env_width / 3) 0 (1 * env_width / 3) env_height;
  let ball_button_x, line_button_x, cup_button_x =
    7 * env_width / 10, 4 * env_width / 5, 9 * env_width / 10
  in
  let ball_button_y, line_button_y, cup_button_y =
    9 * env_height / 10, 9 * env_height / 10, 9 * env_height / 10
  in
  let ball_button_width, line_button_width, cup_button_width =
    1 * env_width / 15, 1 * env_width / 15, 1 * env_width / 15
  in
  let ball_button_height, line_button_height, cup_button_height =
    1 * env_height / 20, 1 * env_width / 15, 1 * env_width / 15
  in
  generate_button
    "Ball"
    ~x_pos:ball_button_x
    ~y_pos:ball_button_y
    ~width:ball_button_width
    ~height:ball_button_height
    ~color_button:white
    ~color_text:black;
  generate_button
    "Line"
    ~x_pos:line_button_x
    ~y_pos:line_button_y
    ~width:line_button_width
    ~height:line_button_height
    ~color_button:white
    ~color_text:black;
  generate_button
    "Cup"
    ~x_pos:cup_button_x
    ~y_pos:cup_button_y
    ~width:cup_button_width
    ~height:cup_button_height
    ~color_button:white
    ~color_text:black;
  Graphics.set_color white;
  Graphics.draw_string "Current Object: Ball"
;;

let rec handle_clicks () =
  let event = wait_next_event [ Button_down ] in
  (* Wait for a mouse click *)
  if event.button
  then (* Check if a button was pressed (i.e., a click occurred) *)
    (
    let click_x = event.mouse_x in
    let click_y = event.mouse_y in
    (* Example: If click is within a specific region, draw a circle *)
    if
      click_x >= ball_button_x
      && click_x < ball_button_x + ball_button_width
      && click_y > ball_button_y - ball_button_height
      && click_y < ball_button_y
    then (
      set_color blue;
      fill_circle click_x click_y 20;
      print_endline "Ball Button Pressed")
    else if
      click_x >= line_button_x
      && click_x < line_button_x + line_button_width
      && click_y > line_button_y - line_button_height
      && click_y < line_button_y
    then ()
    (* Continue handling clicks *))
  else if
    click_x >= cup_button_x
    && click_x < cup_button_x + cup_button_width
    && click_y > cup_button_y - cup_button_height
    && click_y < cup_button_y
  then ()
  (* Continue handling clicks *)
  else handle_clicks () (* If no button down, continue waiting for clicks *)
;;
