open Lexer
open Expr
open Primitives

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  let lex () =
    let l, r = Sedlexing.lexing_positions lexbuf in
    (Lex.lex lexbuf, l, r)
  in
  let parse = MenhirLib.Convert.Simplified.traditional2revised Parser.program in
  let prog =
    try parse lex
    with Parser.Error ->
      let p1, p2 = Sedlexing.lexing_positions lexbuf in
      let l1, c1, l2, c2 =
        ( p1.pos_lnum,
          p1.pos_cnum - p1.pos_bol,
          p2.pos_lnum,
          p2.pos_cnum - p2.pos_bol )
      in
      Printf.printf "Parsing error! This happened on position: %i:%i - %i:%i\n"
        l1 c1 l2 c2;
      exit (-1)
  in
  print_endline "=== AST Dump Start ===";
  Eval.dump prog;
  print_endline "=== AST Dump Done  ===";
  Eval.toplevel Prim.primitives AST.Undefined prog
