open! Core
open! Objects

module Button = struct
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; display_text : string
    }

  let create ~height ~width ~position ~id ~display_text =
    { height; width; position; id; display_text }
  ;;

  let in_bounds t x y =
    let btn_min_x = t.position.x in
    let btn_max_x = btn_min_x + t.width in
    let btn_min_y = t.position.y in
    let btn_max_y = btn_min_y + t.height in
    x >= btn_min_x && x <= btn_max_x && y >= btn_min_y && y <= btn_max_y
  ;;
end

module Panel = struct
  type t =
    { height : int
    ; width : int
    ; buttons : Button.t list
    }

  let create ~height ~width =
    let width = width * 3 in
    let ball_x, line_x, cup_x =
      7 * width / 10, 4 * width / 5, 9 * width / 10
    in
    let y = 9 * height / 10 in
    let box_y = y - 50 in
    let button_width = 1 * width / 15 in
    let button_height = 1 * height / 20 in
    let ball_button =
      Button.create
        ~height:button_height
        ~width:button_width
        ~position:{ x = ball_x; y }
        ~id:"create-ball-btn"
        ~display_text:"Ball"
    in
    let line_button =
      Button.create
        ~height:button_height
        ~width:button_width
        ~position:{ x = line_x; y }
        ~id:"create-line-btn"
        ~display_text:"Line"
    in
    let cup_button =
      Button.create
        ~height:button_height
        ~width:button_width
        ~position:{ x = cup_x; y }
        ~id:"create-cup-btn"
        ~display_text:"Cup"
    in
    let box_button =
      Button.create
        ~height:button_height
        ~width:button_width
        ~position:{ x = ball_x; y = box_y }
        ~id:"create-box-btn"
        ~display_text:"Box"
    in
    { height
    ; width
    ; buttons = [ ball_button; box_button; cup_button; line_button ]
    }
  ;;
end

module Canvas = struct
  type t =
    { height : int
    ; width : int
    ; mutable balls : Ball.t list
    ; mutable lines : Line.t list
    ; mutable cups : Cup.t list
    ; mutable boxes : Box.t list
    }

  let create ~height ~width =
    { height; width; balls = []; lines = []; cups = []; boxes = [] }
  ;;

  let in_bounds t x y = x >= 0 && x < t.width && y >= 0 && y < t.height

  let add_ball t obj =
    t.balls <- t.balls @ [ obj ];
    print_endline "add_ball"
  ;;

  let add_line t obj = t.lines <- t.lines @ [ obj ]
  let add_cup t obj = t.cups <- t.cups @ [ obj ]
  let add_box t obj = t.boxes <- t.boxes @ [ obj ]
end

module UI = struct
  type t =
    { height : int
    ; width : int
    ; panel : Panel.t
    ; canvas : Canvas.t
    }

  let create ~height ~width =
    { height
    ; width
    ; panel = Panel.create ~height ~width:(width / 3)
    ; canvas = Canvas.create ~height ~width:(2 * width / 3)
    }
  ;;
end
