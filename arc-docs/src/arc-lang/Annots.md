# Annotations

Annotations can optionally be attached to items.

```grammar
Annots ::= "@" "{" [[Annot]]","* "}"

Annot ::= [Name] (":" [Value])?
```

## Examples

```arc-lang
{{#include ../../../arc-lang/stdlib/std.arc:unit}}
```
