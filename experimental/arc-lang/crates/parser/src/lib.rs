#![allow(macro_use_extern_crate)]

use ast::Item;
use diagnostics::Diagnostic;
use im_rc::Vector;
use lexer::Lexer;
use info::FileId;

/// Module for representing the context-free grammar of Arc-Script.
pub(crate) mod grammar {
    #![allow(unused_extern_crates)]
    #![allow(unreachable_pub)]
    #![allow(clippy::correctness)]
    #![allow(clippy::style)]
    #![allow(clippy::complexity)]
    #![allow(clippy::perf)]
    #![allow(clippy::pedantic)]
    #![allow(clippy::nursery)]
    #![allow(clippy::cargo)]
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}
/// Module for translating LALRPOP's [`ParsingError`]s to Arc-[`Diagnostics`].
pub(crate) mod error;

pub fn parse(file_id: FileId, source: &str, diagnostics: &mut Vec<Diagnostic>) -> Vector<Item> {
    let mut lexer = Lexer::new(file_id, source);
    let program = grammar::ProgramParser::new()
        .parse(file_id, diagnostics, &mut lexer)
        .unwrap();
    diagnostics.append(&mut lexer.diagnostics());
    program
}
