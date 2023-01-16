{
  open Lexing
  open Token
  open Error

  let info lexbuf = loc (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = pos.pos_cnum;
                pos_lnum = pos.pos_lnum + 1
      }
}

let int = ['0'-'9'] ['0'-'9']*
let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit+ frac? exp?
let percentage = digit+ frac? '%'
let char = [^ '\'' ]
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let name = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*
let datetime = int '-' int '-' int ('T' int ':' int ':' int)?

rule token =
  parse
  | "("        as s { Printf.printf "%c " s; ParenL }
  | ")"        as s { Printf.printf "%c " s; ParenR }
  | "["        as s { Printf.printf "%c " s; BrackL }
  | "]"        as s { Printf.printf "%c " s; BrackR }
  | "{"        as s { Printf.printf "%c " s; BraceL }
  | "}"        as s { Printf.printf "%c " s; BraceR }
  | "<"        as s { Printf.printf "%c " s; AngleL }
  | ">"        as s { Printf.printf "%c " s; AngleR }
(*= Operators =as s ==Printf.printf "%s " s; ===========================================================*)
  | "!="       as s { Printf.printf "%s " s; Neq }
  | "%"        as s { Printf.printf "%c " s; Percent }
  | "*"        as s { Printf.printf "%c " s; Star }
  | "**"       as s { Printf.printf "%s " s; StarStar }
  | "+"        as s { Printf.printf "%c " s; Plus }
  | ","        as s { Printf.printf "%c " s; Comma }
  | "-"        as s { Printf.printf "%c " s; Minus }
  | "."        as s { Printf.printf "%c " s; Dot }
  | ".."       as s { Printf.printf "%s " s; DotDot }
  | "..="      as s { Printf.printf "%s " s; DotDotEq }
  | "/"        as s { Printf.printf "%c " s; Slash }
  | ":"        as s { Printf.printf "%c " s; Colon }
  | "::"       as s { Printf.printf "%s " s; ColonColon }
  | ";"        as s { Printf.printf "%c " s; Semi }
  | "<="       as s { Printf.printf "%s " s; Leq }
  | "="        as s { Printf.printf "%c " s; Eq }
  | "=="       as s { Printf.printf "%s " s; EqEq }
  | "=>"       as s { Printf.printf "%s " s; Imply }
  | ">="       as s { Printf.printf "%s " s; Geq }
  | "_"        as s { Printf.printf "%c " s; Underscore }
  | "|"        as s { Printf.printf "%c " s; Bar }
  | "@"        as s { Printf.printf "%c " s; AtSign }
  | "+="       as s { Printf.printf "%s " s; PlusEq }
  | "-="       as s { Printf.printf "%s " s; MinusEq }
  | "*="       as s { Printf.printf "%s " s; StarEq }
  | "/="       as s { Printf.printf "%s " s; SlashEq }
  | "%="       as s { Printf.printf "%s " s; PercentEq }
  | "~"        as s { Printf.printf "%c " s; Tilde }
  | "()"       as s { Printf.printf "%s " s; Unit }
  | "!"        as s { Printf.printf "%c " s; Never }
(*= Keywords ==as s ==Printf.printf "%s " s; ============================================================*)
  | "and"      as s { Printf.printf "%s " s; And }
  | "as"       as s { Printf.printf "%s " s; As }
  | "band"     as s { Printf.printf "%s " s; Band }
  | "bor"      as s { Printf.printf "%s " s; Bor }
  | "break"    as s { Printf.printf "%s " s; Break }
  | "bxor"     as s { Printf.printf "%s " s; Bxor }
  | "builtin"  as s { Printf.printf "%s " s; Builtin }
  | "case"     as s { Printf.printf "%s " s; Case }
  | "catch"    as s { Printf.printf "%s " s; Catch }
  | "class"    as s { Printf.printf "%s " s; Class }
  | "compute"  as s { Printf.printf "%s " s; Compute }
  | "continue" as s { Printf.printf "%s " s; Continue }
  | "def"      as s { Printf.printf "%s " s; Def }
  | "desc"     as s { Printf.printf "%s " s; Desc }
  | "dict"     as s { Printf.printf "%s " s; Dict }
  | "do"       as s { Printf.printf "%s " s; Do }
  | "dyn"      as s { Printf.printf "%s " s; Dyn }
  | "else"     as s { Printf.printf "%s " s; Else }
  | "extern"   as s { Printf.printf "%s " s; Extern }
  | "false"    as s { Printf.printf "%s " s; Bool false }
  | "finally"  as s { Printf.printf "%s " s; Finally }
  | "for"      as s { Printf.printf "%s " s; For }
  | "from"     as s { Printf.printf "%s " s; From }
  | "fun"      as s { Printf.printf "%s " s; Fun }
  | "group"    as s { Printf.printf "%s " s; Group }
  | "if"       as s { Printf.printf "%s " s; If }
  | "in"       as s { Printf.printf "%s " s; In }
  | "infix"    as s { Printf.printf "%s " s; Infix }
  | "instance" as s { Printf.printf "%s " s; Instance }
  | "into"     as s { Printf.printf "%s " s; Into }
  | "join"     as s { Printf.printf "%s " s; Join }
  | "length"   as s { Printf.printf "%s " s; Length }
  | "loop"     as s { Printf.printf "%s " s; Loop }
  | "match"    as s { Printf.printf "%s " s; Match }
  | "mod"      as s { Printf.printf "%s " s; Mod }
  | "new"      as s { Printf.printf "%s " s; New }
  | "not"      as s { Printf.printf "%s " s; Not }
  | "of"       as s { Printf.printf "%s " s; Of }
  | "on"       as s { Printf.printf "%s " s; On }
  | "or"       as s { Printf.printf "%s " s; Or }
  | "order"    as s { Printf.printf "%s " s; Or }
  | "return"   as s { Printf.printf "%s " s; Return }
  | "select"   as s { Printf.printf "%s " s; Select }
  | "set"      as s { Printf.printf "%s " s; Set }
  | "repeat"   as s { Printf.printf "%s " s; Repeat }
  | "throw"    as s { Printf.printf "%s " s; Throw }
  | "true"     as s { Printf.printf "%s " s; Bool true }
  | "try"      as s { Printf.printf "%s " s; Try }
  | "type"     as s { Printf.printf "%s " s; Type }
  | "use"      as s { Printf.printf "%s " s; Use }
  | "val"      as s { Printf.printf "%s " s; Val }
  | "var"      as s { Printf.printf "%s " s; Var }
  | "where"    as s { Printf.printf "%s " s; Where }
  | "while"    as s { Printf.printf "%s " s; While }
  | "window"   as s { Printf.printf "%s " s; Window }
  | "xor"      as s { Printf.printf "%s " s; Xor }
(*= Identifiers and Literals ================================================*)
  | int as s0 name as s1   { Printf.printf "%s%s" s0 s1; IntSuffix (int_of_string s0, s1) }
  | float as s0 name as s1 { Printf.printf "%s%s" s0 s1; FloatSuffix (float_of_string s0, s1) }
  | int as s               { Printf.printf "%s" s; Int (int_of_string s) }
  | float as s             { Printf.printf "%s" s; Float (float_of_string s) }
  | name as s              { Printf.printf "%s" s; Name s }
  | '\'' char as s '\''    { Printf.printf "%s" s; Char (String.get s 0) }
  | '"'               as s { Printf.printf "%c" s; string (Buffer.create 17) lexbuf }
  | datetime as s          { Printf.printf "%s" s; String s }
  | "#"               as s { Printf.printf "%c" s; line_comment lexbuf }
  | whitespace        as s { Printf.printf "%s" s; token lexbuf }
  | newline           as s { next_line lexbuf; Printf.printf "%s" s; token lexbuf }
  | _ as c                 { Printf.printf "%c" c; raise (LexingError (info lexbuf, Printf.sprintf "Unexpected char: '%c'" c)) }
  | eof                    { Eof }

and line_comment =
  parse
  | newline { next_line lexbuf; token lexbuf }
  | _ { line_comment lexbuf }

and string buf =
  parse
  | '"'                { String (Buffer.contents buf) }
  | '\\' '/'           { Buffer.add_char buf '/'; string buf lexbuf }
  | '\\' '\\'          { Buffer.add_char buf '\\'; string buf lexbuf }
  | '\\' 'b'           { Buffer.add_char buf '\b'; string buf lexbuf }
  | '\\' 'f'           { Buffer.add_char buf '\012'; string buf lexbuf }
  | '\\' 'n'           { Buffer.add_char buf '\n'; string buf lexbuf }
  | '\\' 'r'           { Buffer.add_char buf '\r'; string buf lexbuf }
  | '\\' 't'           { Buffer.add_char buf '\t'; string buf lexbuf }
  | [^ '"' '\\']+ as s { Buffer.add_string buf s; string buf lexbuf }
  | _ as c             { raise (LexingError (info lexbuf, Printf.sprintf "Unexpected char: '%c'" c)) }
  | eof                { raise (LexingError (info lexbuf, "String is not terminated")) }
