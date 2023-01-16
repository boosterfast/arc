%{
%}

%%

%public paren(x): "(" x ")" { $2 }
%public brack(x): "[" x "]" { $2 }
%public brace(x): "{" x "}" { $2 }
%public angle(x): "<" x ">" { $2 }

%public paren_seq(x):
  | paren(seq(x)) { $1 }
  | "()" { [] }

%public %inline epsilon: {}

%public fst(a, b): a b { $1 }
%public snd(a, b): a b { $2 }
%public split(a, b, c): a b c { ($1, $3) }

%public %inline llist(x):
  llist_rev(x) { $1 |> List.rev }

llist_rev(x):
  | epsilon { [] }
  | llist_rev(x) x { $2::$1 }

%public nonempty_llist(x):
  nonempty_llist_rev(x) { $1 |> List.rev }

nonempty_llist_rev(x): 
  | x { [$1] }
  | nonempty_llist_rev(x) x { $2::$1 }

%public %inline separated_nonempty_llist(s, x):
  separated_nonempty_llist_rev(s, x) { $1 |> List.rev }

separated_nonempty_llist_rev(s, x): 
  | x { [$1] }
  | separated_nonempty_llist_rev(s, x) s x { $3::$1 }

%public %inline separated_llist(s, x):
  | epsilon { [] }
  | separated_nonempty_llist(s, x) { $1 }

%public separated_llist_trailing(s, x):
  separated_llist(s, x) s? { $1 }

%public seq(x): separated_llist_trailing(",", x) { $1 }
%public seq_nonempty(x): separated_nonempty_llist(",", x) { $1 }
%public seq_explicit(x): x "," seq(x) { $1::$3 }
