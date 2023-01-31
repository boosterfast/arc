//! Module which generates a `LogosLexer`.

use std::fmt::Debug;
use std::str::FromStr;

use logos::Lexer;
use logos::Logos;

#[rustfmt::skip]
#[derive(Logos, Debug, Clone)]
#[logos(subpattern bin = r"0[bB][0-1][_0-1]*")]
#[logos(subpattern oct = r"0[oB][0-7][_0-7]*")]
#[logos(subpattern dec = r"[0-9][_0-9]*")]
#[logos(subpattern hex = r"0[xX][0-9a-fA-F][_0-9a-fA-F]*")]
#[logos(subpattern exp = r"[eE][+-]?[0-9][_0-9]*")]
#[logos(subpattern id = r"[A-Za-z_][A-Za-z0-9_]*")]
pub enum Token {
    #[token("(")] ParenL,
    #[token(")")] ParenR,
    #[token("[")] BrackL,
    #[token("]")] BrackR,
    #[token("{")] BraceL,
    #[token("}")] BraceR,
    #[token("<")] AngleL,
    #[token(">")] AngleR,
    // Operators
    #[token("!=")] Neq,
    #[token("%")] Percent,
    #[token("*")] Star,
    #[token("**")] StarStar,
    #[token("+")] Plus,
    #[token(",")] Comma,
    #[token("-")] Minus,
    #[token(".")] Dot,
    #[token("..")] DotDot,
    #[token("..=")] DotDotEq,
    #[token("/")] Slash,
    #[token(":")] Colon,
    #[token("::")] ColonColon,
    #[token(";")] Semi,
    #[token("<=")] Leq,
    #[token("=")] Eq,
    #[token("==")] EqEq,
    #[token("=>")] Imply,
    #[token(">=")] Geq,
    #[token("@")] AtSign,
    #[token("_")] Underscore,
    #[token("|")] Bar,
    #[token("+=")] PlusEq,
    #[token("-=")] MinusEq,
    #[token("*=")] StarEq,
    #[token("/=")] SlashEq,
    #[token("%=")] PercentEq,
    #[token("~")] Tilde,
    #[token("!")] Never,
    #[token("()")] Parens,
    // Keywords
    #[token("and")] And,
    #[token("as")] As,
    #[token("break")] Break,
    #[token("band")] Band,
    #[token("bor")] Bor,
    #[token("bxor")] Bxor,
    #[token("builtin")] Builtin,
    #[token("case")] Case,
    #[token("catch")] Catch,
    #[token("class")] Class,
    #[token("continue")] Continue,
    #[token("dict")] Dict,
    #[token("def")] Def,
    #[token("desc")] Desc,
    #[token("do")] Do,
    #[token("dyn")] Dyn,
    #[token("else")] Else,
    #[token("extern")] Extern,
    #[token("finally")] Finally,
    #[token("for")] For,
    #[token("from")] From,
    #[token("fun")] Fun,
    #[token("group")] Group,
    #[token("if")] If,
    #[token("in")] In,
    #[token("infix")] Infix,
    #[token("into")] Into,
    #[token("instance")] Instance,
    #[token("join")] Join,
    #[token("length")] Length,
    #[token("loop")] Loop,
    #[token("match")] Match,
    #[token("mod")] Mod,
    #[token("new")] New,
    #[token("not")] Not,
    #[token("on")] On,
    #[token("or")] Or,
    #[token("of")] Of,
    #[token("order")] Order,
    #[token("return")] Return,
    #[token("compute")] Compute,
    #[token("set")] Set,
    #[token("select")] Select,
    #[token("repeat")] Repeat,
    #[token("throw")] Throw,
    #[token("try")] Try,
    #[token("type")] Type,
    #[token("val")] Val,
    #[token("var")] Var,
    #[token("where")] Where,
    #[token("window")] Window,
    #[token("while")] While,
    #[token("use")] Use,
    #[token("xor")] Xor,
    // Identifiers and Literals
    #[regex(r"(?&id)", |lex| lex.slice().to_string())] Name(String),
    #[regex(r"-?((?&bin)|(?&dec)|(?&hex)|(?&oct))", |lex| lex.slice().parse())] Int(i128),
    #[regex(r"-?((?&bin)|(?&dec)|(?&hex)|(?&oct))(?&id)", Token::parse_suffix)] IntSuffix((i128, String)),
    #[regex(r"-?(((?&dec)\.(?&dec)(?&exp)?)|((?&dec)(?&exp)))", |lex| lex.slice().parse())] Float(f64),
    #[regex(r"-?(((?&dec)\.(?&dec)(?&exp)?)|((?&dec)(?&exp)))(?&id)", Token::parse_suffix)] FloatSuffix((f64, String)),

    #[regex(r#"true|false"#, |lex| lex.slice().parse())] Bool(bool),
    #[regex(r"'[^']'", |lex| lex.slice().parse())] Char(char),
    #[regex(r#""([^"\\]|\\.)*""#, |lex| lex.slice().get(1..lex.slice().len()-1).unwrap_or("").to_string())] String(String),

    #[regex(r"[ \t\f\n]+", logos::skip)]
    #[regex(r"#[^r\n]*", logos::skip, priority = 2)]  // # ...
    #[regex(r"#[^\n]*", logos::skip, priority = 1)]   // # ...
    #[error]
    Error,
}

impl Token {
    fn parse_suffix<'s, T>(lexer: &Lexer<'s, Token>) -> Result<(T, String), <T as FromStr>::Err>
    where
        T: std::str::FromStr,
        <T as FromStr>::Err: Debug,
    {
        let slice = lexer.slice();
        let end = slice.chars().position(|char| !char.is_digit(10)).unwrap();
        let int = slice[..end].parse()?;
        let suffix = slice[end..].to_string();
        Ok((int, suffix))
    }
}
