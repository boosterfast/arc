//! Module for converting errors emitted by `LALRPOP` into compiler diagnostics.

use diagnostics::Error;
use lalrpop_util::ErrorRecovery;
use lalrpop_util::ParseError;
use lexer::tokens::Token;
use info::ByteIndex;
use info::FileId;
use info::Info;

/// Converts an LALRPOP `ErrorRecovery` into a `Diagnostic`.
pub(crate) fn parser_error(recovery: ErrorRecovery<ByteIndex, Token, ()>, file: FileId) -> Error {
    match recovery.error {
        // User errors (lexer errors) are handled by the lexer.
        ParseError::User { error: () } => unreachable!(),
        // Error generated by the parser when it encounters additional, unexpected tokens.
        ParseError::ExtraToken { token: (l, t, r) } => Error::ExtraToken {
            found: format!("{t:?}"),
            loc: Info::from_range(file, l..r),
        },
        // Error generated by the parser when it encounters a token (or EOF) it did not expect.
        ParseError::InvalidToken { location } => Error::InvalidToken {
            loc: Info::from_range(file, location..location),
        },
        // Error generated by the parser when it encounters an EOF it did not expect.
        ParseError::UnrecognizedEOF { location, expected } => Error::UnrecognizedEOF {
            loc: Info::from_range(file, location..location),
            expected,
        },
        // Error generated by the parser when it encounters a token it did not expect.
        ParseError::UnrecognizedToken {
            token: (l, t, r),
            expected,
        } => Error::UnrecognizedToken {
            found: format!("{t:?}"),
            loc: Info::from_range(file, l..r),
            expected,
        },
    }
}
