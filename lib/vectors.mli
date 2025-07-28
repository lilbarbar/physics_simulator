open! Core

module Position : sig
  type t = Vector.t [@@deriving sexp]
end

module Velocity : sig
  type t = Vector.t [@@deriving sexp]
end

module Force : sig
  type t = Vector.t [@@deriving sexp]
end
