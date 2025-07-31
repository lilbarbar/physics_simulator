open! Core
open! Graphics
open! Objects

let draw_ball (ball : Ball.t) =
  let x = Int.of_float ball.center.x in
  let y = Int.of_float ball.center.y in
  let radius = Int.of_float ball.radius in
  Graphics.set_color Colors.blue;
  Graphics.fill_circle x y radius;
  print_endline "draw_ball"
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

let draw_box (box : Box.t) : unit =
  let x1 = Int.of_float box.min.x in
  let y1 = Int.of_float box.min.y in
  let x2 = Int.of_float box.max.x in
  let y2 = Int.of_float box.max.y in
  Graphics.fill_rect x1 y1 (x2 - x1) (y2 - y1)
;;

let draw_objects (t : Interface.Canvas.t) =
  List.iter t.balls ~f:(fun ball -> draw_ball ball);
  List.iter t.lines ~f:(fun line -> draw_line line);
  List.iter t.cups ~f:(fun cup -> draw_cup cup);
  List.iter t.boxes ~f:(fun box -> draw_box box)
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

let create_environment (ui : Interface.UI.t) =
  Graphics.open_graph " 20000 x 20000 ";
  Graphics.resize_window ui.width ui.height;
  Graphics.set_color black;
  Graphics.fill_rect 0 0 ui.width ui.height;
  Graphics.set_color Colors.gray;
  Graphics.fill_rect ui.canvas.width 0 ui.panel.width ui.panel.height;
  List.iter ui.panel.buttons ~f:(fun button ->
    generate_button
      button.id
      ~x_pos:button.position.x
      ~y_pos:button.position.y
      ~width:button.width
      ~height:button.height)
;;

let init_exn (ui : Interface.UI.t) = create_environment ui
let render (canvas : Interface.Canvas.t) = draw_objects canvas
