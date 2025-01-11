module Prim = struct
  open Expr.AST

  let nonsense v name =
    Printf.printf "[Warning: Value %s makes no sense under %s operator!]\n"
      (val_to_string v) name;
    Undefined

  let numOp1 name op v =
    match v with NumVal x -> NumVal (op x) | _ -> nonsense v name

  let numOp2 op v v' =
    match (v, v') with
    | NumVal x, NumVal y -> NumVal (op x y)
    | _ ->
        Printf.printf
          "[Warning: Value %s and Value %s makes no sense under numeric \
           operator!]\n"
          (val_to_string v) (val_to_string v');
        Undefined

  let primitives =
    [
      ( "plus",
        function
        | TupleVal l -> List.fold_left (numOp2 ( +. )) (NumVal 0.) l
        | x -> numOp1 "plus" (fun x -> x) x );
      ( "minus",
        function
        | TupleVal l -> List.fold_left (numOp2 ( -. )) (NumVal 0.) l
        | x -> numOp1 "minus" (fun x -> -.x) x );
      ( "times",
        function
        | TupleVal l -> List.fold_left (numOp2 ( *. )) (NumVal 0.) l
        | x -> numOp1 "times" (fun x -> x) x );
      ( "div",
        function
        | TupleVal l -> List.fold_left (numOp2 ( /. )) (NumVal 0.) l
        | x -> numOp1 "div" (fun x -> 1. /. x) x );
      ( "pow",
        function
        | TupleVal [ a; b ] -> numOp2 ( ** ) a b
        | x -> nonsense x "pow" );
      ( "mod",
        function
        | TupleVal [ a; b ] -> numOp2 mod_float a b
        | x -> nonsense x "mod" );
      ("exp", numOp1 "exp" exp);
      ("log", numOp1 "log" log);
      ("sin", numOp1 "sin" sin);
      ("cos", numOp1 "cos" cos);
      ("tan", numOp1 "tan" tan);
      ("sinh", numOp1 "sinh" sinh);
      ("cosh", numOp1 "cosh" cosh);
      ("tanh", numOp1 "tanh" tanh);
      ("asin", numOp1 "asin" asin);
      ("acos", numOp1 "acos" acos);
      ("atan", numOp1 "atan" atan);
      ("asinh", numOp1 "asinh" asinh);
      ("acosh", numOp1 "acosh" acosh);
      ("atanh", numOp1 "atanh" atanh);
    ]
end
