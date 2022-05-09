# Language Reference

The syntax of Arc-Lang is formalized using a Regex-based variation of the BNF grammar where:

* `+` and `*` denote repetition.
* `?` is for optional rules.
* `(` `)` indicates grouping.
* `|` is for alternation.
* `[` `]` for is character-alternation (e.g., `[abc]`).
* `-` is for ranges (e.g., `[a-zA-Z]`).
* `.` is for matching any character.
* `\` is for escaping characters.
* Non-terminals are written as uppercase (e.g., `Expr`).
* Terminals are written in blue text (e.g., <code><for></code>)
