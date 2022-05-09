# Queries

```grammar
Query ::= "from" ([Pattern] "in" [Expr])","+ "{" [[QueryStmt]]+ "}"

QueryStmt ::=
  | "select" [Expr] ("as" [Name])?                # Select
  | "where" [Expr]                            # Filter
  | "join" ([Pattern] "in" [Expr])","+ "on" [Expr]      # Join
  | "group" [Expr]","+ ("as" [Name])?               # Partition
  | "compute" [Expr] ("of" [Expr])? ("as" [Name])?    # Aggregation
  | "order" [Expr] "desc"?                      # Ordering
  | "window" [Expr] "as" [Name] "{" [[WindowLogic]]+ "}"  # Window
  | "into" [Expr]                             # Piping

WindowLogic ::= [[WindowKind]] "compute" [Expr] ("of" [Expr])? ("as" [Name])?

WindowKind ::=
    | "length" [Expr] ("step" [Expr])?  # Tumbling and sliding window
    | "count" [Expr]                # Count window
```
