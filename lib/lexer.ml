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

  type tok =
    | Tnum of num
    | Tid of id
    | Tlp
    | Trp
    | Tcomma
    | Tplus
    | Tminus
    | Ttimes
    | Tdiv
    | Tpow
    | Tdot
    | Tto
    | Tset
    | Tsep
    | Teof
  [@@deriving sexp_of]

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
    | ' ' | '\t' | '\n' -> lex lexbuf
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
    | '.' -> Tdot
    | ',' -> Tcomma
    | real_lit -> Tnum (lexeme lexbuf |> float_of_string)
    | digit_sequence -> Tnum (lexeme lexbuf |> float_of_string)
    | ident -> Tid (lexeme lexbuf)
    | eof -> Teof
    | _ ->
        Printf.printf "Unexpected Charactor %s at position %i\n" (lexeme lexbuf)
          (Sedlexing.lexeme_start lexbuf);
        failwith "lexer failed!"
end
