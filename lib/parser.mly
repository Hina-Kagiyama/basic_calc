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
%token Teof

%left ";"
%left ":="
%left "->"
%left "+" "-"
%left "*" "/"
%right POS NEG
%right "^"
%nonassoc Tnum Tid
%left Tsel
%right "("

%start <AST.program> program
%{ open AST %}

%%

let program :=
    | l = separated_list(",", exp); ","?; Teof; { Program l }

let snl(a, b) == separated_nonempty_list(a, b)

let exp :=
    | a = exp; "+"; b = exp; { AppExp (VarExp "plus", TupleExp [a; b]) }
    | a = exp; "-"; b = exp; { AppExp (VarExp "minus", TupleExp [a; b]) }
    | a = exp; "*"; b = exp; { AppExp (VarExp "times", TupleExp [a; b]) }
    | a = exp; "/"; b = exp; { AppExp (VarExp "div", TupleExp [a; b]) }
    | a = exp; "^"; b = exp; { AppExp (VarExp "pow", TupleExp [a; b]) }
    | "+"; a = exp; %prec POS { AppExp (VarExp "plus", a) }
    | "-"; a = exp; %prec NEG { AppExp (VarExp "plus", a) }
    | a = exp; b = Tsel; { SelExp (a, b) }
    | a = exp; ";"; b = exp; { SeqExp (a, b) }
    | a = Tid; ":="; b = exp; { SetExp (a, b) }
    | a = exp; b = prim_exp; { AppExp (a, b) }
    | a = Tid; "->"; b = exp; { LamExp (a, b) }
    | a = prim_exp; { a }

let prim_exp :=
    | a = Tid; { VarExp a }
    | "("; a = exp; ")"; { a }
    | "("; a = exp; ","; l = separated_list(",", exp); ")"; { TupleExp (a :: l) }
    | a = Tnum; { NumExp a }

