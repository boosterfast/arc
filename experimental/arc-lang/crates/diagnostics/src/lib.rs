use info::Info;

/// A compile-time or runtime diagnostic.
#[derive(Debug)]
pub enum Diagnostic {
    /// A compile-time info
    Info(Info),
    /// A compile-time warning
    Warning(Warning),
    /// A compile-time error
    Error(Error),
}

impl From<Info> for Diagnostic {
    fn from(info: Info) -> Self {
        Self::Info(info)
    }
}

impl From<Warning> for Diagnostic {
    fn from(warning: Warning) -> Self {
        Self::Warning(warning)
    }
}

impl From<Error> for Diagnostic {
    fn from(error: Error) -> Self {
        Self::Error(error)
    }
}

/// Compile-time info reported by the compiler.
#[derive(Debug)]
pub enum Hint {}

/// Compile-time warnings reported by the compiler.
#[derive(Debug)]
pub enum Warning {}

/// Compile-time errors reported by the compiler.
#[derive(Debug)]
pub enum Error {
    /// Error when the importer fails to find a source file.
    FileNotFound,

    /// Error when the parser comes across an extra token.
    ExtraToken {
        /// Extra token found while parsing.
        found: String,
        /// Location of the error.
        loc: Info,
    },

    /// Error when the parser comes across an invalid token.
    InvalidToken {
        /// Location of the error.
        loc: Info,
    },

    /// Error when the parser comes across an unrecognized end-of-file.
    UnrecognizedEOF {
        /// Location of the error.
        loc: Info,
        /// List of tokens expected by LALRPOP.
        expected: Vec<String>,
    },

    /// Error when the parser comes across an unrecognized token.
    /// In other words, a token emitted by the lexer which the parser
    /// did not expect.
    /// NB: This is technically an Internal-Compiler-Error.
    UnrecognizedToken {
        /// Unrecognized token found while parsing.
        found: String,
        /// Location of the token.
        loc: Info,
        /// List of tokens expected by LALRPOP.
        expected: Vec<String>,
    },

    /// Error when two types fail to unify.
    TypeMismatch {
        /// First type.
        lhs: String,
        /// Second type.
        rhs: String,
        /// Location of the error.
        loc: Info,
    },

    /// Error when type information is needed at a code location, but is not supplied.
    TypeMustBeKnownAtThisPoint {
        /// Location of the error.
        loc: Info,
    },

    /// Error when a path does not reference anything.
    PathNotFound {
        /// Path which failed to resolve.
        path: String,
        /// Location of the error.
        loc: Info,
    },

    /// Error when a match is non-exhaustive.
    NonExhaustiveMatch {
        /// Location of the error.
        loc: Info,
    },

    /// Error when two items in the same namespace have the same name.
    NameClash {
        /// Name of the two items.
        name: String,
    },

    /// Error when a struct contains two fields with the same name.
    FieldClash {
        /// Name of the two fields.
        name: String,
    },

    /// Error when an enum contains two variants with the same name.
    VariantClash {
        /// Name of the two variants.
        name: String,
    },

    /// Error when an enum variant is constructed with too many arguments.
    VariantWrongArity {
        /// Path of the enum variant.
        path: String,
    },

    /// Error when a tuple is indexed with an out-of-bounds index.
    OutOfBoundsProject {
        /// Location of the error.
        loc: Info,
    },

    /// Error when a struct-field is accessed which does not exist.
    FieldNotFound {
        /// Location of the error.
        loc: Info,
    },

    /// Error when placing a type in value position.
    TypeInValuePosition {
        /// Location of the error.
        loc: Info,
    },

    /// Error when placing a value in type position.
    ValueInTypePosition {
        /// Location of the error.
        loc: Info,
    },

    /// Error when enwrapping a non-variant path.
    PathIsNotVariant {
        /// Location of the error.
        loc: Info,
    },

    /// Error when placing a refutable pattern where an irrefutable one is expected.
    RefutablePattern {
        /// Location of the error.
        loc: Info,
    },

    /// Error when moving a used value, and the moved value does not implement Copy.
    UseOfMovedValue {
        /// Location of the parent value.
        loc0: Info,
        /// Location of the second value.
        loc1: Info,
        /// Type which does not implement Copy.
        t: String,
    },

    /// Error when using the same value twice, and the used value does not implement Copy.
    DoubleUse {
        /// Location of the place expression declaration.
        loc0: Info,
        /// Location of the first use.
        loc1: Info,
        /// Location of the second use.
        loc2: Info,
        /// Type which does not implement Copy.
        t: String,
    },

    /// Error when an extern function contains a parameter which is a pattern.
    PatternInExternFun {
        /// Location of the extern function.
        loc: Info,
    },

    /// Error when an expression expects a selector (e.g., e1 not in e2[123]), but the selector is
    /// not specified.
    ExpectedSelector {
        /// Location of the expression
        loc: Info,
    },

    /// Error when multiple selectors are specified. For now this is a hard-error, but will relaxed
    /// in the future.
    MultipleSelectors {
        /// Location of the expression
        loc: Info,
    },

    /// Error when using a non-selectable type (map, set) in a selector.
    ExpectedSelectableType {
        /// Location of the expression
        loc: Info,
    },
}
