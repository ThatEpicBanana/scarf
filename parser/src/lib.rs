// temporary
//TODO: remove this
#![allow(dead_code)]

use std::fmt::{self, Debug};

use chumsky::{prelude::*, Stream, chain::Chain};

/// A type representing a span of input text 
pub type Span = std::ops::Range<usize>;
/// A type representing an optional AST node, with Err holding a [`Span`]
// pub type Opt<T> = Result<T, Span>;

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Opt<T> {
    Ok(T),
    Err(Span),
}

impl<T: Debug> fmt::Debug for Opt<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self {
            Ok(x) => 
                if f.alternate() { write!(f, "{:#?}", x) } 
                else { write!(f, "{:?}", x) }
            Err(x) => 
                if f.alternate() { write!(f, "Error: {:#?}", x) } 
                else { write!(f, "Error: {:?}", x) },
        }
    }
}


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
    pub out: Option<Vec<Item>>,
    pub lexer_errors: Vec<Simple<char>>,
    pub parser_errors: Vec<Simple<Token>>,
}

pub fn parse<'a, Iter, S>(input: S) -> ParserOutput 
where 
    Iter: Iterator<Item = (char, Span)> + 'a,
    S: Into<Stream<'a, char, Span, Iter>>,
{ parse_inner(input, false) }

pub fn parse_verbose<'a, Iter, S>(input: S) -> ParserOutput 
where 
    Iter: Iterator<Item = (char, Span)> + 'a,
    S: Into<Stream<'a, char, Span, Iter>>,
{ parse_inner(input, true) }

fn parse_inner<'a, Iter, S>(input: S, verbose: bool) -> ParserOutput 
where 
    Iter: Iterator<Item = (char, Span)> + 'a,
    S: Into<Stream<'a, char, Span, Iter>>,
{
    // length required for stream for some reason, probably for errors
    let len = input.len();

    // try lexing
    //TODO: make it so this only works in test environments
    let (out, lexer_errors) = 
        if verbose { lexer::create().parse_recovery_verbose(input) }
        else       { lexer::create().parse_recovery        (input) };

    // check output of lexer
    let (out, parser_errors) = if let Some(out) = out {
        // if lexer succeeds, try parsing
        if verbose { crate::create().parse_recovery_verbose(Stream::from_iter(len..len + 1, out.into_iter())) }
        else       { crate::create().parse_recovery        (Stream::from_iter(len..len + 1, out.into_iter()))}
    } else { 
        // if lexer fails, output lexer errors
        return ParserOutput{out: None, lexer_errors, parser_errors: Vec::new()};
    };

    // final output
    ParserOutput{out, lexer_errors, parser_errors}
}

