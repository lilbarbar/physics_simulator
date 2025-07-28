open! Core

module Position = struct
  include Vector [@@deriving sexp]
end

module Velocity = struct
  include Vector [@@deriving sexp]
end

module Force = struct
  include Vector [@@deriving sexp]
end
