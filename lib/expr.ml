(*
Abstract syntax:

expr ::= expr "+" expr
       | expr "-" expr
       | expr "*" expr
       | expr "/" expr
       | expr "^" expr
       | "+" expr
       | "-" expr
       | expr expr
       | "(" expr ")"
       | "(" expr "," exprList ")"
       | ident
       | real_lit
exprList ::= expr
           | exprList "," exprList
 *)

module AST = struct
  open Common.Defs
  open Sexplib0.Sexp_conv

  type exp =
    | AppExp of exp * exp
    | VarExp of id
    | NumExp of num
    | TupleExp of exp list
    | SelExp of exp * int
    | LamExp of id * exp
    | SeqExp of exp * exp
    | SetExp of id * exp
    | Arg
  [@@deriving sexp_of]

  let to_string t = t |> sexp_of_exp |> Sexplib0.Sexp.to_string
end

module Env = struct
  open Common.Defs
  open Sexplib0.Sexp_conv
  module Tbl = Hashtbl.Make (String)

  type value =
    | Undefined
    | None
    | NumVal of num
    | FunVal of AST.exp
    | TupleVal of value list
  [@@deriving sexp_of]

  type env = { arg : value; tbl : value Tbl.t }

  let lookup (e : env) = Tbl.find_opt e.tbl
  let set (e : env) = Tbl.add e.tbl
  let call (e : env) v = { arg = v; tbl = Tbl.copy e.tbl }
  let get_arg (e : env) = e.arg
  let val_to_string v = v |> sexp_of_value |> Sexplib0.Sexp.to_string
end

module Eval = struct
  open Common.Defs
  open AST
  open Env

  exception ExternVarUsed of id

  let rec eval env : exp -> value = function
    | NumExp v -> NumVal v
    | VarExp id -> (
        match Env.lookup env id with
        | Some x -> x
        | None ->
            Printf.printf "[Warning: Variable %s not found!]\n" id;
            Undefined)
    | TupleExp l -> TupleVal (l |> List.map (eval env))
    | AppExp (fexp, farg) -> (
        match eval env fexp with
        | FunVal fexp ->
            let farg = eval env farg in
            eval (Env.call env farg) fexp
        | v ->
            Printf.printf
              "[Warning: Value %s from Expression %s not applicatable!]\n"
              (Env.val_to_string v) (to_string fexp);
            Undefined)
    | SelExp (texp, k) -> (
        match eval env texp with
        | TupleVal l -> List.nth l k
        | v ->
            Printf.printf
              "[Warning: Value %s from Expression %s not indexable!]\n"
              (Env.val_to_string v) (to_string texp);
            Undefined)
    | SetExp (id, vexp) ->
        let vexp = eval env vexp in
        Env.set env id vexp;
        None
    | SeqExp (a, b) -> (
        match eval env a with
        | None -> eval env b
        | v ->
            Printf.printf
              "[Warning: Value %s from Expression %s not discardable!]\n"
              (Env.val_to_string v) (to_string a);
            Undefined)
    | LamExp (id, bexp) ->
        let rec rewrite = function
          | VarExp id' when id' = id -> Arg
          | VarExp id' -> raise @@ ExternVarUsed id'
          | SelExp (a, b) -> SelExp (rewrite a, b)
          | AppExp (a, b) -> AppExp (rewrite a, rewrite b)
          | SetExp (a, b) -> SetExp (a, rewrite b)
          | SeqExp (a, b) -> SeqExp (rewrite a, rewrite b)
          | TupleExp l -> TupleExp (List.map rewrite l)
          | x -> x
        in
        FunVal (rewrite bexp)
    | Arg -> Env.get_arg env

  let toplevel (arg : value) (explst : exp list) : unit =
    let env = Env.{ arg; tbl = Tbl.create 255 } in
    explst
    |> List.iter (fun e ->
           eval env e |> Env.val_to_string |> Printf.printf "=> %s\n")
end
