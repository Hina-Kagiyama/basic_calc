module Prim = struct
  open Expr.AST

  let nonsense v name =
    Printf.printf "[Warning: Value %s makes no sense under function '%s'!]\n"
      (val_to_string v) name;
    Undefined

  let numOp1 name op v =
    match v with NumVal x -> NumVal (op x) | _ -> nonsense v name

  let numOp2 name op v v' =
    match (v, v') with
    | NumVal x, NumVal y -> NumVal (op x y)
    | _ ->
        Printf.printf
          "[Warning: Value %s and Value %s make no sense under function '%s'!]\n"
          (val_to_string v) (val_to_string v') name;
        Undefined

  let cmpOp2 name op = function
    | TupleVal (a :: l) -> (
        let rec aux acc a = function
          | [] -> acc
          | b :: l -> aux (op a b && acc) b l
        in
        match aux true a l with true -> None | false -> Undefined)
    | x -> nonsense x name

  let primitives =
    [
      ( "+",
        function
        | TupleVal (a :: l) -> List.fold_left (numOp2 "+" ( +. )) a l
        | x -> numOp1 "+" (fun x -> x) x );
      ( "-",
        function
        | TupleVal (a :: l) -> List.fold_left (numOp2 "-" ( -. )) a l
        | x -> numOp1 "-" (fun x -> -.x) x );
      ( "*",
        function
        | TupleVal (a :: l) -> List.fold_left (numOp2 "*" ( *. )) a l
        | x -> numOp1 "*" (fun x -> x) x );
      ( "/",
        function
        | TupleVal (a :: l) -> List.fold_left (numOp2 "/" ( /. )) a l
        | x -> numOp1 "/" (fun x -> 1. /. x) x );
      ( "^",
        function
        | TupleVal [ a; b ] -> numOp2 "^" ( ** ) a b
        | x -> nonsense x "^" );
      ( "mod",
        function
        | TupleVal [ a; b ] -> numOp2 "mod" mod_float a b
        | x -> nonsense x "mod" );
      ("=", cmpOp2 "=" ( = ));
      ("!=", cmpOp2 "!=" ( <> ));
      ("<", cmpOp2 "<" ( < ));
      (">", cmpOp2 ">" ( > ));
      ("<=", cmpOp2 "<=" ( <= ));
      (">=", cmpOp2 ">=" ( >= ));
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
