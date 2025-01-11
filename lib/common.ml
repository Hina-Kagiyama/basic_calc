module Defs = struct
  open Sexplib0.Sexp_conv

  type id = string [@@deriving sexp_of]
  type num = float [@@deriving sexp_of]
end
