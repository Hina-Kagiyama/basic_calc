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
    | Tquest
    | Tarrow
    | Teq
    | Tneq
    | Tlt
    | Tgt
    | Tle
    | Tge
    | Teol
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

  let ident =
    [%sedlex.regexp? alpha, Star (alpha | digit_sequence | Chars "_'")]

  open Tok
  open Sedlexing.Utf8
  open Common

  exception UnBalancedParen

  let make_lexer lexbuf =
    let lv = ref 0 in
    let rec aux () =
      match%sedlex lexbuf with
      | ' ' | '\t' | '%', Star (Compl '\n'), "\n" -> aux ()
      | "!=", Opt '\n' -> Tneq
      | "->", Opt '\n' -> Tto
      | ":=", Opt '\n' -> Tset
      | "<=", Opt '\n' -> Tle
      | "=>", Opt '\n' -> Tarrow
      | ">=", Opt '\n' -> Tge
      | "<", Opt '\n' -> Tlt
      | "=", Opt '\n' -> Teq
      | ">", Opt '\n' -> Tgt
      | '*', Opt '\n' -> Ttimes
      | '+', Opt '\n' -> Tplus
      | ',', Opt '\n' -> Tcomma
      | '-', Opt '\n' -> Tminus
      | '/', Opt '\n' -> Tdiv
      | ';', Opt '\n' -> Tsep
      | '?', Opt '\n' -> Tquest
      | '^', Opt '\n' -> Tpow
      | '.', digit_sequence ->
          let s = lexeme lexbuf in
          Tsel (String.sub s 1 (String.length s - 1) |> int_of_string)
      | real_lit -> Tnum (lexeme lexbuf |> float_of_string)
      | digit_sequence -> Tnum (lexeme lexbuf |> float_of_string)
      | '_' -> Tid "_"
      | ident -> Tid (lexeme lexbuf)
      | '(' ->
          incr lv;
          Tlp
      | ')' ->
          decr lv;
          if !lv >= 0 then Trp else raise UnBalancedParen
      | '\n' -> if !lv = 0 then Teol else aux ()
      | eof -> Teof
      | _ ->
          Printf.printf "Unexpected Charactor %s at position " (lexeme lexbuf);
          report_pos lexbuf;
          failwith "lexer failed!"
    in
    aux
end
