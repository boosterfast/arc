use std::ops::Range;

use text_size::TextRange;
use text_size::TextSize;

/// A structure which keeps the start and end position of an AST node plus its source file.
pub type Spanned<T> = (T, Info);

/// An index of a character in a source file.
pub type ByteIndex = TextSize;

/// A span between two characters in a source file.
pub type Span = TextRange;

/// An identifier of a source file. Can be used to access the source code of the source file.
pub type FileId = usize;

/// Stores a code location.
#[derive(Debug, Clone, Copy)]
pub enum Info {
    /// A real location which maps to a span in a source file.
    Loc(FileId, Span),
    /// A fake location, synthesized during compilation.
    Gen,
}

impl Info {
    pub fn new(file: FileId, start: ByteIndex, end: ByteIndex) -> Self {
        Self::Loc(file, Span::new(start, end))
    }

    /// Constructs a source file from a file and a byte index range.
    pub fn from_range(file: FileId, range: Range<ByteIndex>) -> Self {
        Self::Loc(file, Span::new(range.start, range.end))
    }

    /// Joins two locations into a potentially larger location.
    pub fn join(self, other: Self) -> Self {
        match (self, other) {
            (Info::Loc(file, span0), Info::Loc(_, span1)) => Self::Loc(file, span0.cover(span1)),
            (loc @ Info::Loc(..), Info::Gen) | (Info::Gen, loc @ Info::Loc(..)) => loc,
            (Info::Gen, Info::Gen) => Info::Gen,
        }
    }
}

