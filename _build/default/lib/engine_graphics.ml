open! Core

module Position = struct
  type t =
    { row : int
    ; col : int
    }
end

module Colors = struct
  let black = Graphics.rgb 000 000 000
  let green = Graphics.rgb 000 255 000
  let head_color = Graphics.rgb 100 100 125
  let red = Graphics.rgb 255 000 000
  let white = Graphics.rgb 255 255 255
  let game_in_progress = Graphics.rgb 100 100 200
  let game_lost = Graphics.rgb 200 100 100
  let game_won = Graphics.rgb 100 200 100
  let gold = Graphics.rgb 255 223 0
end

module Constants = struct
  let scaling_factor = 1.
  let play_area_height = 600. *. scaling_factor |> Float.iround_down_exn
  let header_height = 75. *. scaling_factor |> Float.iround_down_exn
  let play_area_width = 675. *. scaling_factor |> Float.iround_down_exn
  let block_size = 27. *. scaling_factor |> Float.iround_down_exn
end

let only_one : bool ref = ref false

let init_exn () =
  let open Constants in
  (* Should raise if called twice *)
  if !only_one
  then failwith "Can only call init_exn once"
  else only_one := true;
  Graphics.open_graph
    (Printf.sprintf
       " %dx%d"
       (play_area_height + header_height)
       play_area_width);
  let height = play_area_height / block_size in
  let width = play_area_width / block_size in
  Engine.create ~height ~width ~initial_snake_length:3
;;

let draw_block { Position.row; col } ~color ~outline =
  let open Constants in
  let col = col * block_size in
  let row = row * block_size in
  Graphics.set_color color;
  let x1, y1, x2, y2 = col + 1, row + 1, block_size - 1, block_size - 1 in
  Graphics.fill_rect x1 y1 x2 y2;
  if outline
  then (
    Graphics.set_color Graphics.cyan;
    Graphics.draw_rect x1 y1 x2 y2)
;;

let draw_header ~engine_state =
  let open Constants in
  let header_color =
    match (engine_state : Engine_state.t) with
    | In_progress -> Colors.game_in_progress
    | Failure | Paused -> Colors.game_lost
    | Clear -> Colors.game_won
  in
  Graphics.moveto 0 (play_area_height + 50);
  Graphics.set_color header_color;
  Graphics.fill_rect 0 play_area_height play_area_width header_height;
  let header_text = Engine_state.to_string engine_state in
  Graphics.set_color Colors.black;
  Graphics.moveto 0 play_area_height;
  Graphics.draw_string header_text;
  Graphics.moveto 0 (play_area_height + 25)
;;

(* match spelled_words with
   | [] -> ()
   | hd :: tl ->
   Graphics.draw_string "Spelled words: ";
   Graphics.set_color Colors.red;
   Graphics.draw_string (hd ^ " ");
   Graphics.set_color Colors.black;
   Graphics.draw_string (String.concat tl ~sep:" ")
   ;; *)

let draw_play_area () =
  let open Constants in
  Graphics.set_color Colors.black;
  Graphics.fill_rect 0 0 play_area_width play_area_height
;;

(* let draw_letter letter ~color =
  let open Constants in
  let ({ col; row } : Position.t) = Letter.position letter in
  let char_width, char_height =
    Letter.char letter |> String.of_char |> Graphics.text_size
  in
  let col = (col * block_size) + ((block_size - char_width) / 2) in
  let row = (row * block_size) + ((block_size - char_height) / 2) in
  Graphics.moveto col row;
  Graphics.set_color color;
  Graphics.draw_char (Letter.char letter)
;; *)

(* let draw_letters (letters : Letters.t) =
   Letters.letters letters
   |> List.iter ~f:(fun letter ->
   draw_letter letter ~color:(Colors.letter_color letter))
   ;; *)

(* let draw_snake snake_head snake_tail eaten_letters =
   let () =
   match eaten_letters with
   | None ->
   List.iter snake_tail ~f:(draw_block ~color:Colors.green ~outline:false)
   | Some eaten_letters ->
   let pos_and_chars, _ =
   List.zip_with_remainder (List.rev snake_tail) eaten_letters
   in
   List.iter pos_and_chars ~f:(fun (position, char) ->
   draw_block position ~color:Colors.green ~outline:true;
   Letter.create ~position ~char |> draw_letter ~color:Colors.black)
   in
   (* Snake head is a different color *)
   draw_block ~color:Colors.head_color snake_head ~outline:false
   ;; *)

let render engine =
  Graphics.display_mode false;
  let engine_state = Engine.engine_state engine in
  draw_header ~engine_state;
  draw_play_area ();
  Graphics.display_mode true;
  Graphics.synchronize ()
;;

let read_key () =
  if Graphics.key_pressed () then Some (Graphics.read_key ()) else None
;;
