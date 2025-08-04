open! Core
open! Graphics
open! Objects

module TextBox = struct
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; mutable display_text : string
    }

  let create ~height ~width ~position ~id ~display_text =
    { height; width; position; id; display_text }
  ;;

  let update_text t ~(display_text : string) = t.display_text <- display_text
end

module Button = struct
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; mutable display_text : string
    ; mutable color : Graphics.color
    }

  let create ~height ~width ~position ~id ~display_text ~color =
    { height; width; position; id; display_text; color }
  ;;

  let in_bounds t x y =
    let btn_min_x = t.position.x in
    let btn_max_x = btn_min_x + t.width in
    let btn_min_y = t.position.y in
    let btn_max_y = btn_min_y + t.height in
    x >= btn_min_x && x <= btn_max_x && y >= btn_min_y && y <= btn_max_y
  ;;
end

type t =
  { height : int
  ; width : int
  ; buttons : Button.t list
  ; text_boxes : TextBox.t list
  }

let create ~height:ui_height ~width =
  let create_btn_width = width * 3 / 15 in
  let create_btn_height = ui_height / 20 in
  let base_y = 9 * ui_height / 10 in
  let box_y = base_y - 50 in
  let create_button ~x ~y ~id ~text ~color =
    Button.create
      ~height:create_btn_height
      ~width:create_btn_width
      ~position:{ x; y }
      ~id
      ~display_text:text
      ~color
  in
  let create_ball_btn =
    create_button
      ~x:(7 * width * 3 / 10)
      ~y:base_y
      ~id:"create-ball-btn"
      ~text:"Ball"
      ~color:Graphics.white
  in
  let create_line_btn =
    create_button
      ~x:(4 * width * 3 / 5)
      ~y:base_y
      ~id:"create-line-btn"
      ~text:"Line"
      ~color:Graphics.white
  in
  let create_cup_btn =
    create_button
      ~x:(9 * width * 3 / 10)
      ~y:base_y
      ~id:"create-cup-btn"
      ~text:"Cup"
      ~color:Graphics.white
  in
  let create_box_btn =
    create_button
      ~x:(7 * width * 3 / 10)
      ~y:box_y
      ~id:"create-box-btn"
      ~text:"Box"
      ~color:Graphics.white
  in
  let clear_btn =
    Button.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:{ x = 7 * width * 3 / 10; y = box_y - 50 }
      ~id:"clear-btn"
      ~display_text:"Clear"
      ~color:Graphics.red
  in
  let play_pause_button =
    Button.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:{ x = 7 * width * 3 / 10; y = box_y - 100 }
      ~id:"play-pause-btn"
      ~display_text:"Play"
      ~color:Graphics.white
  in
  let click_state_text =
    TextBox.create
      ~height:create_btn_height
      ~width:(8 * width / 10)
      ~position:{ x = 7 * width * 3 / 10; y = box_y - 150 }
      ~id:"click_state_text"
      ~display_text:""
  in
  { height = ui_height
  ; width
  ; buttons =
      [ create_ball_btn
      ; create_box_btn
      ; create_cup_btn
      ; create_line_btn
      ; clear_btn
      ; play_pause_button
      ]
  ; text_boxes = [ click_state_text ]
  }
;;
