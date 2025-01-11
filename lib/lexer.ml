(*
real_lit = digit_sequence decimal_exponent
         | digit_sequence "." decimal_exponent?
         | digit_sequence? "." digit_sequence decimal_exponent?
digit_sequence = [0-9]+
decimal_exponent = [eE] [+-]? digit_sequence

operator = [+-*/^]
left_paren = "("
right_paren = ")"

alpha = [A-Za-z]
ident = alpha (alpha | digit_sequence)*
*)

module Tok = struct
  open Common.Defs
  open Sexplib0.Sexp_conv

  type tok =
    | Tnum of num
    | Tid of id
    | Tsel of int
    | Tlp
    | Trp
    | Tcomma
    | Tplus
    | Tminus
    | Ttimes
    | Tdiv
    | Tpow
    | Tto
    | Tset
    | Tsep
    | Teof
  [@@deriving sexp_of]

  type token = tok

  let to_string t = t |> sexp_of_tok |> Sexplib0.Sexp.to_string
end

module Lex = struct
  let digit_sequence = [%sedlex.regexp? Plus '0' .. '9']

  let decimal_exponent =
    [%sedlex.regexp? Chars "eE", Opt (Chars "+-"), digit_sequence]

  let real_lit =
    [%sedlex.regexp?
      ( digit_sequence, decimal_exponent
      | digit_sequence, ".", Opt decimal_exponent
      | Opt digit_sequence, ".", digit_sequence, Opt decimal_exponent )]

  let alpha = [%sedlex.regexp? 'A' .. 'Z' | 'a' .. 'z']
  let ident = [%sedlex.regexp? alpha, Star (alpha | digit_sequence)]

  open Tok
  open Sedlexing.Utf8

  let rec lex lexbuf =
    match%sedlex lexbuf with
    | Opt ',', Star '\n', eof -> Teof
    | ' ' | '\t' | '\n' | '%', Star (Compl '\n'), '\n' -> lex lexbuf
    | ';' -> Tsep
    | "->" -> Tto
    | ":=" -> Tset
    | '(' -> Tlp
    | ')' -> Trp
    | '+' -> Tplus
    | '-' -> Tminus
    | '*' -> Ttimes
    | '/' -> Tdiv
    | '^' -> Tpow
    | Star '\n', ',' -> Tcomma
    | '#', digit_sequence ->
        let s = lexeme lexbuf in
        Tsel (String.sub s 1 (String.length s - 1) |> int_of_string)
    | real_lit -> Tnum (lexeme lexbuf |> float_of_string)
    | digit_sequence -> Tnum (lexeme lexbuf |> float_of_string)
    | ident -> Tid (lexeme lexbuf)
    | _ ->
        Printf.printf "Unexpected Charactor %s at position %i\n" (lexeme lexbuf)
          (Sedlexing.lexeme_start lexbuf);
        failwith "lexer failed!"
end
