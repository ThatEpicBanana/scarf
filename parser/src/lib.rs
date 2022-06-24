// temporary
// TODO: remove this
#![allow(dead_code)]

use chumsky::{prelude::*, Stream, chain::Chain};

pub mod lexer;
pub mod parser;

#[cfg(test)] 
pub mod tests;

// TODO: overhaul errors

use crate::parser::prelude::*;

pub struct ParserOutput {
    pub out: Option<Vec<Item>>,
    pub lexer_errors: Vec<Simple<char>>,
    pub parser_errors: Vec<Simple<Token>>,
}

fn parse<'a, Iter, S>(input: S) -> ParserOutput 
where 
    Iter: Iterator<Item = (char, Span)> + 'a,
    S: Into<Stream<'a, char, Span, Iter>>,
{
    // length required for stream for some reason, probably for errors
    let len = input.len();

    // try lexing
    cfg_if::cfg_if!(
        if #[cfg(test)] { let (out, lexer_errors) = lexer::create().parse_recovery_verbose(input); }
        else            { let (out, lexer_errors) = lexer::create().parse_recovery        (input); }
    );

    // get output from lexer or return errors
    let out = match out {
        Some(x) => x,
        None => return ParserOutput{out: None, lexer_errors, parser_errors: Vec::new()},
    };

    // try parsing
    cfg_if::cfg_if!(
        if #[cfg(test)] { let (out, parser_errors) = parser::create().parse_recovery_verbose(Stream::from_iter(len..len + 1, out.into_iter())); }
        else            { let (out, parser_errors) = parser::create().parse_recovery        (Stream::from_iter(len..len + 1, out.into_iter())); }
    );

    // final output
    ParserOutput{out, lexer_errors, parser_errors}
}

