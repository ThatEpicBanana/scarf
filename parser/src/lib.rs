use chumsky::{prelude::*, Stream, chain::Chain};

/// A type representing a span of input text 
pub type Span = std::ops::Range<usize>;
/// A type representing an optional AST node, with Err holding a [`Span`]
pub type Opt<T> = Result<T, Span>;

pub mod lexer;
mod parser;

#[cfg(test)] mod tests;

use crate::parser::prelude::*;

pub fn create() -> impl Parser<Token, Vec<Item>, Error = Simple<Token>> {
    item::item()
        .repeated()
        .then_ignore(end())
}

pub struct ParserOutput {
    out: Option<Vec<Item>>,
    lexer_errors: Vec<Simple<char>>,
    parser_errors: Vec<Simple<Token>>,
}

pub fn parse<'a, Iter, S>(input: S) -> ParserOutput 
where 
    Iter: Iterator<Item = (char, Span)> + 'a,
    S: Into<Stream<'a, char, Span, Iter>>,
{
    // length required for stream for some reason, probably for errors
    let len = input.len();

    // try lexing
    let (out, lexer_errors) = 
        lexer::create().parse_recovery(input);

    // check output of lexer
    let (out, parser_errors) = if let Some(out) = out {
        // if lexer succeeds, try parsing
        crate::create().parse_recovery(Stream::from_iter(len..len + 1, out.into_iter()))
    } else { 
        // if lexer fails, output lexer errors
        return ParserOutput{out: None, lexer_errors, parser_errors: Vec::new()};
    };

    // final output
    ParserOutput{out, lexer_errors, parser_errors}
}

