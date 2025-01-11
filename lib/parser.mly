%{
    open Common.Defs
    open Expr
%}

%token <num> Tnum
%token <id> Tid
%token <int> Tsel
%token Tlp "("
%token Trp ")"
%token Tcomma ","
%token Tplus "+"
%token Tminus "-"
%token Ttimes "*"
%token Tdiv "/"
%token Tpow "^"
%token Tto "->"
%token Tset ":="
%token Tsep ";"
%token Tquest "?"
%token Tarrow "=>"
%token Teq "="
%token Tneq "!="
%token Tlt "<"
%token Tgt ">"
%token Tle "<="
%token Tge ">="
%token Teol
%token Teof

%left ";"
%left ":="
%left "=" "!=" "<" ">" "<=" ">="
%left "->"
%left "+" "-"
%left "*" "/"
%right POS NEG
%right "^"
%nonassoc Tnum Tid
%left Tsel
%right "("

%start <AST.exp list> program
%start <AST.exp> repl_expression
%{ open AST %}

%%

let program := l = separated_list (Teol, exp); Teof; { l }
let repl_expression :=
    | a = exp; Teof; { a }
    | a = exp; Teol; { a }

let snl(a, b) == separated_nonempty_list(a, b)

let exp :=
    | a = exp; "+"; b = exp; { AppExp (VarExp "+", TupleExp [a; b]) }
    | a = exp; "-"; b = exp; { AppExp (VarExp "-", TupleExp [a; b]) }
    | a = exp; "*"; b = exp; { AppExp (VarExp "*", TupleExp [a; b]) }
    | a = exp; "/"; b = exp; { AppExp (VarExp "/", TupleExp [a; b]) }
    | a = exp; "^"; b = exp; { AppExp (VarExp "^", TupleExp [a; b]) }
    | a = exp; "="; b = exp; { AppExp (VarExp "=", TupleExp [a; b]) }
    | a = exp; "!="; b = exp; { AppExp (VarExp "!=", TupleExp [a; b]) }
    | a = exp; "<"; b = exp; { AppExp (VarExp "<", TupleExp [a; b]) }
    | a = exp; ">"; b = exp; { AppExp (VarExp ">", TupleExp [a; b]) }
    | a = exp; "<="; b = exp; { AppExp (VarExp "<=", TupleExp [a; b]) }
    | a = exp; ">="; b = exp; { AppExp (VarExp ">=", TupleExp [a; b]) }
    | "+"; a = exp; %prec POS { AppExp (VarExp "+", a) }
    | "-"; a = exp; %prec NEG { AppExp (VarExp "-", a) }
    | a = exp; b = Tsel; { SelExp (a, b) }
    | a = exp; ";"; b = exp; { SeqExp (a, b) }
    | a = Tid; ":="; b = exp; { SetExp (a, b) }
    | a = exp; b = prim_exp; { AppExp (a, b) }
    | a = Tid; "->"; b = exp; { LamExp (a, b) }
    | a = prim_exp; { a }
    | "?"; "("; l = snl(",", condbr); ")"; { CondExp l }

let prim_exp :=
    | a = Tid; { VarExp a }
    | "("; a = exp; ")"; { a }
    | "("; a = exp; ","; l = separated_list(",", exp); ")"; { TupleExp (a :: l) }
    | a = Tnum; { NumExp a }

let condbr :=
    | a = exp; "=>"; b = exp; { (a, b) }

