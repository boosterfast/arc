%token ParenL "("
%token ParenR ")"
%token BrackL "["
%token BrackR "]"
%token PoundBraceL "#{"
%token BraceL "{"
%token BraceR "}"
%token AngleL "<"
%token AngleR ">"
(*= Operators ==============================================================*)
%token Bang "!"
%token Neq "!="
%token Percent "%"
%token Star "*"
%token StarStar "**"
%token Plus "+"
%token Comma ","
%token Minus "-"
%token Dot "."
%token DotDot ".."
%token DotDotEq "..="
%token Slash "/"
%token Colon ":"
%token ColonColon "::"
%token Semi ";"
%token Leq "<="
%token Geq ">="
%token Eq "="
%token EqEq "=="
%token Imply "=>"
%token Underscore "_"
%token Bar "|"
%token AtSign "@"
(*= Float extensions ========================================================*)
%token NeqDot "!=."
%token PercentDot "%." 
%token StarStarDot "**."
%token StarDot "*." 
%token PlusDot "+." 
%token MinusDot "-." 
%token SlashDot "/." 
%token LeqDot "<=."
%token EqEqDot "==."
%token GeqDot ">=."
%token LtDot "<."
%token GtDot ">."
(*= Keywords ================================================================*)
%token And "and"
%token As "as"
%token Break "break"
%token Band "band"
%token Bor "bor"
%token Bxor "bxor"
%token Class "class"
%token Continue "continue"
%token Def "def"
%token Desc "desc"
%token Duration "duration"
%token Else "else"
%token Enum "enum"
%token Extern "extern"
%token For "for"
%token From "from"
%token Fun "fun"
%token Group "group"
%token If "if"
%token In "in"
%token Instance "instance"
%token Join "join"
%token Loop "loop"
%token Match "match"
%token Mod "mod"
%token Not "not"
%token On "on"
%token Of "of"
%token Or "or"
%token Order "order"
%token Return "return"
%token Reduce "reduce"
%token Step "step"
%token Task "task"
%token Type "type"
%token Val "val"
%token Var "var"
%token Where "where"
%token While "while"
%token Window "window"
%token Use "use"
%token Xor "xor"
%token Yield "yield"
(*= Identifiers and Literals ================================================*)
%token <string> Name
%token <int> Int 
%token <float> Float
%token <bool> Bool
%token <char> Char
%token Unit "unit"
%token <string> String
(*= Special =================================================================*)
%token Eof
%%
