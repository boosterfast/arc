lexer grammar ArcLexerRules;


fragment Digit : [0-9];
fragment BinaryDigit : [0-1];
fragment HexDigit : [0-9a-fA-F];
// Tokens
TIf : 'if';
TIterate : 'iterate';
TLet : 'let';
TFor : 'for';
TMerge : 'merge';
TResult : 'result';
TMacro : 'macro';
TI8 : 'i8';
TI16 : 'i16';
TI32 : 'i32';
TI64 :'i64';
TU8 : 'u8';
TU16 : 'u16';
TU32 : 'u32';
TU64 : 'u64';
TF32 : 'f32';
TF64 : 'f64';
TBool : 'bool';
TUnit : 'unit';
TVec : 'vec';
TStream : 'stream';
TDict : 'dict';
TAppender : 'appender';
TStreamAppender : 'streamappender';
TMerger : 'merger';
TDictMerger : 'dictmerger';
TGroupMerger : 'groupmerger';
TVecMerger : 'vecmerger';
TToVec : 'tovec';
TZip : 'zip';
TScalarIter : 'iter';
TNext : 'next';
TSimdIter : 'simditer';
TFringeIter : 'fringeiter';
TNdIter : 'nditer';
TRangeIter : 'rangeiter';
TNextIter : 'nextiter';
TLen : 'len';
TLookup : 'lookup';
TKeyExists : 'keyexists';
TSlice : 'slice';
TSort : 'sort';
TExp : 'exp';
TSin : 'sin';
TCos : 'cos';
TTan : 'tan';
TASin : 'asin';
TACos : 'acos';
TATan : 'atan';
TSinh : 'sinh';
TCosh : 'cosh';
TTanh : 'tanh';
TLog : 'log';
TErf : 'erf';
TSqrt : 'sqrt';
TCUDF : 'cudf';
TSimd : 'simd';
TSelect : 'select';
TBroadcast : 'broadcast';
TSerialize : 'serialize';
TDeserialize : 'deserialize';
TMin : 'min';
TMax : 'max';
TPow : 'pow';
TPlus : '+';
TMinus : '-';
TStar : '*';
TSlash : '/';
TPercent : '%';
TBar : '|';
TAt : '@';
TQuestion : '?';
TBang : '!';
TEqualEqual : '==';
TEqual : '=';
TNotEqual : '!=';
TLessThan : '<';
TGreaterThan : '>';
TLEq : '<=';
TGEq : '>=';
TAndAnd : '&&';
TBarBar : '||';
TAnd : '&';
TCirc : '^';
// Literals
TBoolLit : 'true'|'false';
TStringLit : '"' .*? '"'; 
// Regions
Comment : '#' .*? ('\n'|EOF) -> channel(HIDDEN); // comments
TIdentifier : [A-Za-z_][A-Za-z0-9_]+ ;             // match identifiers
Whitespace : [ \t\r\n]+ -> channel(HIDDEN) ; // skip spaces, tabs, newlines, \r (Windows)