# Expressions

An **expression** is syntactic construct which can be evaluated into a **value**.

```grammar
Expr ::=
  | [Name]                # Variable reference
  | [Path] ("::" [TypeArgs])  # Item reference
  | [Value]               # Value literal
  | [Query]               # Queries
  | "_"                   # Placeholder
  | "new" [Name] (":" [Expr])?             # Variant-construction
  | "{" [[ExprRecordField]]","* ("|" [Expr])? "}"  # Record-construction
  | "dyn" "{" [[ExprRecordField]]","* ("|" [Expr])? "}"  # Dynamically-typed record
  | "[" [Expr]","* ("|" [Expr])? "]"           # Array-construction
  | [Expr]? ".." ("="? [Expr])?            # Range-construction
  | "fun" [Params] (":" [Type])? "=" [Expr]    # Lambda-function construction
  | [Expr] [[BinOp]] [Expr]   # Binary operation
  | [Expr] [[UnOp]]         # Unary operator
  | [Expr] "(" [Expr]","* ")"   # Function call
  | [Expr] "." [Name]       # Field projection
  | [Expr] "." [0-9]+     # Index projection
  | [Expr] "." "{" [Name]","+ "}" # Fields projection
  | [Expr] "." "(" ([0-9]+)","+ ")" # Indexes projection
  | [Expr] ":" [Type]      # Type annotated
  | "if" [Expr] [Block] ("else" [Block])?            # If-else-expression
  | "match" [Expr] [Arms]                        # Match-expression
  | "for" [Pattern] "in" [Expr] [Block]              # For-loop
  | "while" [Expr] [Block]                       # While-loop
  | "loop" [Block]                             # Infinite loop
  | "break" [Expr]? | "continue" | "return" [Expr]?  # Jumps
  | "try" [Expr] "catch" [Arms] ("finally" [Expr])?    # Exceptions

UnOp ::=
  | "-"    # Arithmetic negation
  | "not"  # Logical negation

BinOp ::=
  | "+"   | "-"  | "*"   | "/"    | "**"  | "%"     # Arithmetic
  | "=="  | "!=" | "<"   | ">"    | "<="  | ">="    # Equality and comparison
  | "and" | "or" | "xor" | "band" | "bor" | "bxor"  # Logical and bitwise
  | "in"  | "not" "in"                              # Contains

ExprRecordField ::=
  | [Name] ":" [Expr]
  | [Expr]
  | [Expr] "." [Name]
```

## Operators

Operators are defined as follows, with precedence from highest to lowest:

| Operator                                       | Arity   | Affix   | Associativity | Overloadable? |
| ---------------------------------------------- | -----   | -----   | ------------- | ------------  |
| **`return` `break`**                           | Unary   | Prefix* |               | No            |
| **`fun` `task` `on`**                          | Unary   | Prefix  |               | No            |
| `=` `!` `+=` `-=` `%=` `*=` `/=` `**=`         | Binary  | Infix   | None          | No            |
| `in` `not in`                                  | Binary  | Infix   | Left          | No            |
| `..` `..=`                                     | Binary  | Infix   | None          | No            |
| **`and` `or` `xor` `bor` `band` `bxor`**       | Binary  | Infix   | Left          | Yes           |
| `==` `!=`                                      | Binary  | Infix   | None          | No            |
| `<` `>` `<=` `>=`                              | Binary  | Infix   | None          | No            |
| `-` `+` `%`                                    | Binary  | Infix   | Left          | Yes           |
| `*` `/`                                        | Binary  | Infix   | Left          | Yes           |
| `**`                                           | Binary  | Infix   | Right         | Yes           |
| **`not`** `-`                                  | Unary   | Prefix  |               | Yes           |
| **`as`**                                       | Binary  | Infix   | Left          | No            |
| `(exprs)` `[exprs]`                            | Unary   | Postfix |               | No            |
| `.index` `.name` `.name(exprs)` `.name[exprs]` | Unary   | Postfix |               | No            |
| Primary expressions                            | Nullary |         |               | No            |

(*) Operand is optional.

## Builtin Functions

The builtin functions of Arc-Lang are listed here.

```arc-lang
{{#exec grep -F 'extern def' ../arc-lang/stdlib/stdlib.arc}}
```

## Examples

### Basic function calls

```arc-lang
{{#include ../../../arc-lang/examples/basic.arc:example}}
```

### Lambda functions

```arc-lang
{{#include ../../../arc-lang/examples/lambda.arc:example}}
```

### Binary operators

```arc-lang
{{#include ../../../arc-lang/examples/binops.arc:example}}
```

### Placeholders

An expression-argument such as `_ + _` desugars into a lambda function `fun(x0, x1): x0 + x1`

```arc-lang
{{#include ../../../arc-lang/examples/placeholder.arc:example}}
```

### Binary operator lifting

Binary operators can be lifted into functions.

```arc-lang
{{#include ../../../arc-lang/examples/binopref.arc:example}}
```

### String interpolation

String interpolation is supported using the `$` and `${}` syntax.


```arc-lang
{{#include ../../../arc-lang/examples/interpolate.arc:example}}
```

### Query

```arc-lang
{{#include ../../../arc-lang/examples/query.arc:implicit}}
```
