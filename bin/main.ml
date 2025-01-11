open Lexer

let () =
  let lexbuf = Sedlexing.Utf8.from_channel stdin in
  let rec loop () =
    match Lex.lex lexbuf with
    | Tok.Teof -> ()
    | tk ->
        Printf.printf "%s " (Tok.to_string tk);
        loop ()
  in
  loop () |> print_newline
