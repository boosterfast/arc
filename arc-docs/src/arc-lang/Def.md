# Functions

Functions are written with the `def` keyword.

```grammar
Def ::= "def" [Name] [Generics]? [Params] ":" [Type] [Where]? = [Body]
```

## Examples

### Functional-style

```arc-lang
{{#include ../../../arc-lang/examples/fib-functional.arc:example}}
```

### Imperative-style

```arc-lang
{{#include ../../../arc-lang/examples/fib-imperative.arc:example}}
```

### Use-before-declare

It's possible to use functions before they're declared:

```arc-lang
{{#include ../../../arc-lang/examples/even-odd.arc:example}}
```
