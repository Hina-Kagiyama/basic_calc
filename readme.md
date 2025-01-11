# Basic Calculator

## Syntax Draft

### Lexcial Definition

```regex
real_lit = digit_sequence decimal_exponent
         | digit_sequence "." decimal_exponent?
         | digit_sequence? "." digit_sequence decimal_exponent?
digit_sequence = [0-9]+
decimal_exponent = [eE] [+-]? digit_sequence

operator = [+-*/^]
left_paren = "("
right_paren = ")

alpha = [A-Za-z]
ident = alpha (alpha | digit_sequence)*

selector = "#" [0-9]
```

### Syntax Definition

```bnf
expr ::= expr "+" expr
       | expr "-" expr
       | expr "*" expr
       | expr "/" expr
       | expr "^" expr
       | ident "->" expr
       | ident ":=" expr
       | expr selector
       | expr ";" expr
       | "+" expr
       | "-" expr
       | expr expr
       | "(" expr ")"
       | "(" expr "," exprList ")"
       | ident
       | real_lit
exprList ::= expr
           | exprList "," exprList
```

