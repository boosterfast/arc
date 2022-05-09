# Arms

Arms are used for conditional branching inside `match` and `try` expressions.

```grammar
Arms ::= "{" [[Arm]]","* "}"

Arm ::=
  | [Expr] ("if" [Expr])? "=>" [Expr]
```
