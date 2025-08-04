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
    ; display_text : string
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
  let ui_width = width * 3 in
  let create_ball_btn_x, create_line_btn_x, create_cup_btn_x =
    7 * ui_width / 10, 4 * ui_width / 5, 9 * ui_width / 10
  in
  let create_ball_line_cup_btn_y = 9 * ui_height / 10 in
  let create_box_btn_y = create_ball_line_cup_btn_y - 50 in
  let create_btn_width = ui_width / 15 in
  let create_btn_height = ui_height / 20 in
  let create_ball_btn =
    Button.create
      ~height:create_btn_height
      ~width:create_btn_width
      ~position:{ x = create_ball_btn_x; y = create_ball_line_cup_btn_y }
      ~id:"create-ball-btn"
      ~display_text:"Ball"
      ~color:Graphics.white
  in
  let create_line_btn =
    Button.create
      ~height:create_btn_height
      ~width:create_btn_width
      ~position:{ x = create_line_btn_x; y = create_ball_line_cup_btn_y }
      ~id:"create-line-btn"
      ~display_text:"Line"
      ~color:Graphics.white
  in
  let create_cup_btn =
    Button.create
      ~height:create_btn_height
      ~width:create_btn_width
      ~position:{ x = create_cup_btn_x; y = create_ball_line_cup_btn_y }
      ~id:"create-cup-btn"
      ~display_text:"Cup"
      ~color:Graphics.white
  in
  let create_box_btn =
    Button.create
      ~height:create_btn_height
      ~width:create_btn_width
      ~position:{ x = create_ball_btn_x; y = create_box_btn_y }
      ~id:"create-box-btn"
      ~display_text:"Box"
      ~color:Graphics.white
  in
  let clear_btn_width = 8 * width / 10 in
  let clear_btn_height = create_btn_height in
  let clear_btn_x = create_ball_btn_x in
  let clear_btn_y = create_box_btn_y - 50 in
  let clear_btn =
    Button.create
      ~height:clear_btn_height
      ~width:clear_btn_width
      ~position:{ x = clear_btn_x; y = clear_btn_y }
      ~id:"clear-btn"
      ~display_text:"Clear"
      ~color:Graphics.red
  in
  let click_state_text_x = create_ball_btn_x in
  let click_state_text_y = clear_btn_y - 50 in
  let click_state_text_width = clear_btn_width in
  let click_state_text_height = clear_btn_height in
  let click_state_text =
    TextBox.create
      ~height:click_state_text_height
      ~width:click_state_text_width
      ~position:{ x = click_state_text_x; y = click_state_text_y }
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
      ]
  ; text_boxes = [ click_state_text ]
  }
;;
