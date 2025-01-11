module AST = struct
  open Common.Defs
  open Sexplib0.Sexp_conv

  type exp =
    | AppExp of exp * exp
    | LamExp of id * exp
    | NumExp of num
    | SelExp of exp * int
    | SeqExp of exp * exp
    | SetExp of id * exp
    | TupleExp of exp list
    | CondExp of (exp * exp) list
    | VarExp of id
  [@@deriving sexp_of]

  and value =
    | FunVal of id * exp
    | None
    | NumVal of num
    | PrimVal of (value -> value)
    | TupleVal of value list
    | Undefined
  [@@deriving sexp_of]

  type program = exp list

  let rec to_string = function
    | AppExp (a, b) -> "(" ^ to_string a ^ to_string b ^ ")"
    | LamExp (a, b) -> "(" ^ a ^ "->" ^ to_string b ^ ")"
    | NumExp a -> string_of_float a
    | SelExp (a, b) -> "(" ^ to_string a ^ "#" ^ string_of_int b ^ ")"
    | SeqExp (a, b) -> "(" ^ to_string a ^ ";" ^ to_string b ^ ")"
    | SetExp (a, b) -> "(" ^ a ^ ":=" ^ to_string b ^ ")"
    | TupleExp l -> "(" ^ (l |> List.map to_string |> String.concat ",") ^ ")"
    | CondExp l ->
        "(?"
        ^ (l
          |> List.map (fun (a, b) -> to_string a ^ "=>" ^ to_string b)
          |> String.concat "|")
        ^ ")"
    | VarExp a -> a

  let to_human_readable t = t |> sexp_of_exp |> Sexplib0.Sexp.to_string_hum

  let rec val_to_string = function
    | FunVal (a, b) -> a ^ "->" ^ to_string b
    | None -> "[None]"
    | NumVal a -> string_of_float a
    | PrimVal _ -> "[Primitive]"
    | TupleVal l ->
        "(" ^ (l |> List.map val_to_string |> String.concat ",") ^ ")"
    | Undefined -> "[Undefined]"
end

module Env = struct
  open AST
  module Tbl = Hashtbl.Make (String)

  type env = value Tbl.t

  let lookup (e : env) = Tbl.find_opt e
  let add (e : env) = Tbl.add e
  let remove (e : env) = Tbl.remove e
  let has (e : env) = Tbl.mem e

  let call (e : env) v =
    let e' = Tbl.copy e in
    Tbl.add e' "$arg" v;
    e'

  let get_arg (e : env) = Tbl.find e "$arg"

  let create arg =
    let t = Tbl.create 255 in
    Tbl.add t "$arg" arg;
    t

  let dump (e : env) =
    let p name v = Printf.printf "%s = %s\n" name (val_to_string v) in
    Tbl.iter p e
end

module Eval = struct
  open Common.Defs
  open AST

  let rewrite id arg : exp -> exp =
    let rec aux = function
      | AppExp (a, b) -> AppExp (aux a, aux b)
      | NumExp _ as a -> a
      | SelExp (a, b) -> SelExp (aux a, b)
      | SeqExp (a, b) -> SeqExp (aux a, aux b)
      | SetExp (a, b) -> SetExp (a, aux b)
      | TupleExp l -> TupleExp (List.map (fun e -> aux e) l)
      | VarExp i when i = id -> arg
      | VarExp _ as a -> a
      | LamExp (i, _) as a when i = id -> a
      | LamExp (i, e) -> LamExp (i, aux e)
      | CondExp l -> CondExp (List.map (fun (a, b) -> (aux a, aux b)) l)
    in
    aux

  let rec eval env : exp -> value = function
    | NumExp v -> NumVal v
    | VarExp "_" -> None
    | VarExp id -> (
        match Env.lookup env id with
        | Some x -> x
        | None ->
            Printf.printf "[Warning: Variable %s not found!]\n" id;
            Undefined)
    | TupleExp l -> TupleVal (l |> List.map (eval env))
    | AppExp (fexp, farg) -> (
        match eval env fexp with
        | PrimVal prim ->
            let farg = eval env farg in
            prim farg
        | FunVal (id, exp) -> eval env (rewrite id farg exp)
        | v ->
            Printf.printf
              "[Warning: Value %s from Expression %s not applicatable!]\n"
              (val_to_string v) (to_string fexp);
            Undefined)
    | SelExp (texp, k) -> (
        match eval env texp with
        | TupleVal l -> (
            try List.nth l (k + 1)
            with Failure _ ->
              Printf.printf
                "[Warning: This tuple from Expression %s was too short.]\n"
                (to_string texp);
              Undefined)
        | v ->
            Printf.printf
              "[Warning: Value %s from Expression %s not indexable!]\n"
              (val_to_string v) (to_string texp);
            Undefined)
    | SetExp (id, vexp) ->
        let vexp = eval env vexp in
        Env.add env id vexp;
        None
    | SeqExp (a, b) -> (
        match eval env a with
        | None -> eval env b
        | v ->
            Printf.printf
              "[Warning: Value %s from Expression %s not discardable!]\n"
              (val_to_string v) (to_string a);
            Undefined)
    | LamExp (id, bexp) -> FunVal (id, bexp)
    | CondExp l ->
        let rec switch = function
          | [] -> Undefined
          | (a, b) :: l -> if eval env a = None then eval env b else switch l
        in
        switch l

  let make_env (preload : (id * (value -> value)) list) (arg : value) : Env.env
      =
    let env = Env.create arg in
    Env.Tbl.add_seq env
      (preload |> List.to_seq |> Seq.map (fun (a, b) -> (a, PrimVal b)));
    env

  let script (env : Env.env) (prog : program) : unit =
    prog
    |> List.iter (fun e ->
           eval env e |> val_to_string |> Printf.printf "=> %s\n")

  let repl (env : Env.env) (e : exp) : unit =
    eval env e |> val_to_string |> Printf.printf "=> %s\n"

  let dump p = p |> List.iter (fun x -> x |> to_human_readable |> print_endline)
end
