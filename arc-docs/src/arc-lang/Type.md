# Types

All expressions in Arc-Lang have a statically inferred data type. Types of items and variables can be inferred, and thus do not need to be annotated unless desired.

```grammar
Type ::=
  | "{" ([Name] ":" [Type])","* ("|" [Type])? "}"       # Record-type
  | "<" ([Name] ":" [Type])","* ("|" [Type])? ">"       # Enum-type
  | "(" [Type]","* ")"                          # Tuple-type
  | "fun" "(" [Type]","* ")" ":" [Type] ("~" [Effects])?  # Function-type (with optional effects)
  | "[" [Type] "]"                            # Array-type
  | [Type] "?"                              # Option-type
  | [Path] ("[" [Type]","+ "]")?                  # Nominal-type (with optional type parameters)
```

## Examples

Some examples of different types:

```arc-lang
{{#include ../../../arc-lang/examples/types.arc:example}}
```

# Standard types

The following types are provided in the [standard library](https://github.com/cda-group/arc/blob/master/arc-lang/stdlib/std.arc) of Arc-Lang:

```arc-lang
{{#exec grep -F 'extern type' ../arc-lang/stdlib/std.arc}}
```

# Nominal types

Nominal types are context-sensitive. They could either be type aliases, extern types, or generics.
