open! Core
open! Objects

module Button = struct
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    }

  let create ~height ~width ~position ~id = { height; width; position; id }

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
    let ball_x, line_x, cup_x =
      7 * width / 10, 4 * width / 5, 9 * width / 10
    in
    let y = 9 * height / 10 in
    let button_width = 1 * width / 15 in
    let button_height = 1 * height / 20 in
    let ball_button =
      Button.create
        ~height:button_height
        ~width:button_width
        ~position:{ x = ball_x; y }
        ~id:"create-ball-btn"
    in
    let line_button =
      Button.create
        ~height:button_height
        ~width:button_width
        ~position:{ x = line_x; y }
        ~id:"create-line-btn"
    in
    let cup_button =
      Button.create
        ~height:button_height
        ~width:button_width
        ~position:{ x = cup_x; y }
        ~id:"create-cup-btn"
    in
    { height; width; buttons = [ ball_button; line_button; cup_button ] }
  ;;
end

module Canvas = struct
  type t =
    { height : int
    ; width : int
    ; balls : Ball.t list
    ; lines : Line.t list
    ; cups : Cup.t list
    }

  let create ~height ~width =
    { height; width; balls = []; lines = []; cups = [] }
  ;;

  let in_bounds t x y = x >= 0 && x < t.width && y >= 0 && y < t.height
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
