pub mod tokens;

use std::convert::TryInto;

use diagnostics::Diagnostic;
use diagnostics::Error;
use info::ByteIndex;
use info::Info;
use tokens::Token;

pub struct Lexer<'a> {
    inner: logos::Lexer<'a, Token>,
    file_id: usize,
    diagnostics: Vec<Diagnostic>,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (ByteIndex, Token, ByteIndex);

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(token) = self.inner.next() {
                let span = self.inner.span();
                let start = span.start.try_into().unwrap();
                let end = span.end.try_into().unwrap();
                if let Token::Error = token {
                    let loc = Info::new(self.file_id, start, end);
                    self.diagnostics.push(Error::InvalidToken { loc }.into());
                } else {
                    return Some((start, token, end));
                }
            } else {
                return None;
            }
        }
    }
}

impl<'a> Lexer<'a> {
    pub fn new(file_id: usize, source: &'a str) -> Self {
        Self {
            inner: logos::Lexer::new(source),
            file_id,
            diagnostics: Vec::new(),
        }
    }

    pub fn diagnostics(self) -> Vec<Diagnostic> {
        self.diagnostics
    }
}
