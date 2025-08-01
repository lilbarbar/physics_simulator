open! Core
open! Objects

module Button : sig
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; display_text : string
    ; mutable color : Graphics.color
    }

  val create
    :  height:int
    -> width:int
    -> position:Vector.Plain.t
    -> id:string
    -> display_text:string
    -> color:Graphics.color
    -> t

  val in_bounds : t -> int -> int -> bool
end

module TextBox : sig
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    ; mutable display_text : string
    }

  val create
    :  height:int
    -> width:int
    -> position:Vector.Plain.t
    -> id:string
    -> display_text:string
    -> t

  val update_text : t -> display_text:string -> unit
end

module Panel : sig
  type t =
    { height : int
    ; width : int
    ; buttons : Button.t list
    ; text_boxes : TextBox.t list
    }

  val create : height:int -> width:int -> t
end

module Canvas : sig
  type t =
    { height : int
    ; width : int
    ; mutable balls : Ball.t list
    ; mutable lines : Line.t list
    ; mutable cups : Cup.t list
    ; mutable boxes : Box.t list
    }

  val create : height:int -> width:int -> t
  val in_bounds : t -> int -> int -> bool
  val add_ball : t -> Ball.t -> unit
  val add_line : t -> Line.t -> unit
  val add_box : t -> Box.t -> unit
  val add_cup : t -> Cup.t -> unit
  val clear : t -> unit
end

module UI : sig
  type t =
    { height : int
    ; width : int
    ; panel : Panel.t
    ; canvas : Canvas.t
    }

  val create : height:int -> width:int -> t
end
