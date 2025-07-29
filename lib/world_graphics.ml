open! Core
open! Graphics
open! Objects

let draw_ball (ball : Ball.t) =
  let x = Int.of_float ball.center.x in
  let y = Int.of_float ball.center.y in
  let radius = Int.of_float ball.radius in
  Graphics.fill_circle x y radius
;;

let draw_line (line : Line.t) : unit =
  let x1 = Int.of_float line.first_endp.x in
  let y1 = Int.of_float line.first_endp.y in
  let x2 = Int.of_float line.second_endp.x in
  let y2 = Int.of_float line.second_endp.y in
  Graphics.moveto x1 y1;
  Graphics.lineto x2 y2
;;

let draw_cup (cup : Cup.t) : unit =
  let x1 = Int.of_float cup.min.x in
  let y1 = Int.of_float cup.min.y in
  let x2 = Int.of_float cup.max.x in
  let y2 = Int.of_float cup.max.y in
  Graphics.moveto x1 y1;
  Graphics.lineto x1 y2;
  Graphics.lineto x2 y2;
  Graphics.lineto x2 y1
;;

let draw_objects (t : World.t) =
  List.iter t.balls ~f:(fun ball -> draw_ball ball);
  List.iter t.lines ~f:(fun line -> draw_line line);
;;

let generate_button text ~x_pos ~y_pos ~width ~height =
  Graphics.set_color Colors.white;
  Graphics.fill_rect x_pos y_pos width height;
  let text_width, text_height = Graphics.text_size text in
  Graphics.moveto
    (x_pos + (width / 2) - (text_width / 2))
    (y_pos + (height / 2) - (text_height / 2));
  Graphics.set_color Colors.black;
  Graphics.draw_string text
;;

let create_environment ?(width = 750) ?(height = 500) () =
  Graphics.open_graph " 20000 x 20000 ";
  Graphics.resize_window width height;
  Graphics.set_color black;
  Graphics.fill_rect 0 0 width height;
  Graphics.set_color Colors.gray;
  Graphics.fill_rect (2 * width / 3) 0 (1 * width / 3) height;
  let ball_button_x, line_button_x, cup_button_x =
    7 * width / 10, 4 * width / 5, 9 * width / 10
  in
  let ball_button_y, line_button_y, cup_button_y =
    9 * height / 10, 9 * height / 10, 9 * height / 10
  in
  let ball_button_width, line_button_width, cup_button_width =
    1 * width / 15, 1 * width / 15, 1 * width / 15
  in
  let button_height = 1 * height / 20 in
  generate_button
    "Ball"
    ~x_pos:ball_button_x
    ~y_pos:ball_button_y
    ~width:ball_button_width
    ~height:button_height;
  generate_button
    "Line"
    ~x_pos:line_button_x
    ~y_pos:line_button_y
    ~width:line_button_width
    ~height:button_height;
  generate_button
    "Cup"
    ~x_pos:cup_button_x
    ~y_pos:cup_button_y
    ~width:cup_button_width
    ~height:button_height
;;

let init_exn () = create_environment ()
let render world = ignore world