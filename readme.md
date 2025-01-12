# Basic Calculator

I have nothing much to say, but for examples, see `examples/*`.

## Syntax

- Assignment Expression: `id := exp`, id can be any identifier, expressioncan be any expression.
- Lambda Expression: `id -> exp`, id is the formal parameter, exp is the lambda body.
- Basis Operators: prefix operators `+`, `-` and binary operators `+`, `-`, `*`, `/`, `^` and compare operators `=`, `!=`, `<`, `>`, `<=`, `>=` are supported.
- Condition Expression: `? ( pred1 => exp1, pred2 => exp2, ... )` are supported. See `examples/example5.e`.
- Tuple Expression: `(exp1, exp2, ...)` constructs a tuple value.
- Select Expression: `exp .n`, here exp must be a tuple value, and n must be an integer following a dot tightly.
- With `;` at end of a line to suppress repl output.
- Special identifier `_` stands for an always-true condition, also the value of `[None]`.
- Any invaild operation throws an Warning and causes its result to be `[Undefined]`. See `examples/example1.e`.
- Some primitives like `log`, `exp` and all the trigonometric functions are provided. You can also add new primitives though `lib/primitives.ml`.
- End of line triggers evaluation, yet eol after certain operators are ignored. And also eol will be ignored in more than 0 parenthesis level, see `examples/example5.e`.