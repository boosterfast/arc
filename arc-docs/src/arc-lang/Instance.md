# Type Class Instances

Type class instances are written with the `instance` keyword.

```grammar
Instance ::= "instance" [Generics] [Path] [TypeArgs]? [Where]? "{" [Def]","+ "}"
```

## Examples

```arc-lang
{{#include ../../../arc-lang/examples/type-class.arc:instance}}
```
