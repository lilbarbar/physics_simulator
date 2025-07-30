open! Core
open! Objects

module Button : sig
  type t =
    { height : int
    ; width : int
    ; position : Vector.Plain.t
    ; id : string
    }

  val create
    :  height:int
    -> width:int
    -> position:Vector.Plain.t
    -> id:string
    -> t
end

module Panel : sig
  type t =
    { height : int
    ; width : int
    ; buttons : Button.t list
    }

  val create : height:int -> width:int -> t
end

module Canvas : sig
  type t =
    { height : int
    ; width : int
    ; balls : Ball.t list
    ; lines : Line.t list
    ; cups : Cup.t list
    }

  val create : height:int -> width:int -> t
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
