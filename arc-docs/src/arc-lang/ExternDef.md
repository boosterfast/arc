# Extern Functions

An **extern function** is a function-declaration whose implementation is defined externally, outside of Arc-Lang, inside Rust. Extern functions can be annotated with effects that indicate side effects that occur when they are called.

```grammar
ExternDef ::= "extern" "def" [Name] [Generics]? "(" [Type]","* ")" ":" [Type] ("~" [Effects])? [Where]? ";"
```

## Examples

```arc-lang
{{#include ../../../arc-lang/stdlib/std.arc:string}}
```
