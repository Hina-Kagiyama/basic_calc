open Lexer
open Expr
open Primitives
open Common

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  let lex = Lex.make_lexer lexbuf in
  let lex () =
    let l, r = Sedlexing.lexing_positions lexbuf in
    match lex () with Teof -> raise Exit | v -> (v, l, r)
  in
  let parseline () =
    MenhirLib.Convert.Simplified.traditional2revised Parser.repl_expression lex
  in
  let env = Eval.make_env Prim.primitives AST.Undefined in
  let rec repl () =
    (* print_string "> "; *)
    Out_channel.flush stdout;
    try
      let t = parseline () in
      (* print_string "= "; *)
      t |> Eval.repl env;
      repl ()
    with
    | Parser.Error ->
        Printf.printf "Parsing error! This happened on position: ";
        report_pos lexbuf;
        repl ()
    | Lex.UnBalancedParen ->
        Printf.printf "Lexing error! UnBalanced Parentheses at position: ";
        report_pos lexbuf;
        repl ()
    | Exit -> print_newline ()
  in
  repl ()
