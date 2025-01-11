module Defs = struct
  open Sexplib0.Sexp_conv

  type id = string [@@deriving sexp_of]
  type num = float [@@deriving sexp_of]
end

let report_pos lexbuf =
  let p1, p2 = Sedlexing.lexing_positions lexbuf in
  let l1, c1, l2, c2 =
    ( p1.pos_lnum,
      p1.pos_cnum - p1.pos_bol,
      p2.pos_lnum,
      p2.pos_cnum - p2.pos_bol )
  in
  Printf.printf "%i:%i - %i:%i\n" l1 c1 l2 c2

