open! Core
open! Objects

let draw_ball (ball : Ball.t) =
  Graphics.draw_circle ball.x_pos ball.y_pos 50;
  print_string "Done!"
;;

let draw_line (line : Line.t) =
  Graphics.moveto line.x1_pos line.y1_pos;
  Graphics.lineto line.x2_pos line.y2_pos;
  print_string "Done!"
;;

let draw_cup (cup : Cup.t) =
  Graphics.moveto cup.x_pos cup.y_pos;
  Graphics.lineto cup.x_pos (cup.y_pos + 100);
  Graphics.moveto cup.x_pos (cup.y_pos + 100);
  Graphics.lineto (cup.x_pos + 75) (cup.y_pos + 100);
  Graphics.moveto (cup.x_pos + 75) (cup.y_pos + 100);
  Graphics.lineto (cup.x_pos + 75) cup.y_pos;
  print_string "Done!"
;;
