// temporary
//TODO: remove this
#![allow(dead_code)]

use std::fmt::{self, Debug};
use std::ops::Deref;

use chumsky::{prelude::*, Stream, chain::Chain};



pub mod lexer;
mod parser;

#[cfg(test)] 
mod tests;




/// A type representing a span of input text 
pub type Span = std::ops::Range<usize>;

/// A simple (smart pointer) struct that associates a type with a [`Span`]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T>(T, Option<Span>);
/// A shorter alias for [`Spanned`]
pub type S<T> = Spanned<T>;

impl<T> Spanned<T> {
    /// Returns a clone of the held span
    pub fn span(&self) -> Option<Span> {
        self.1.clone()
    }

    pub fn      value(&self) -> &T { &self.0 }
    pub fn take_value( self) ->  T {  self.0 }
    
    /// Creates a [`Spanned`] from a given value and optional span
    pub fn new(value: T, span: Option<Span>) -> Self {
        Spanned(value, span)
    }
    
    // man i'm getting jamais vu
    /// Creates a [`Spanned`] from a value and some span
    pub fn spanned(value: T, span: Span) -> Self {
        Self::new(value, Some(span))
    } 

    /// Creates a [`Spanned`] from a value with no span
    pub fn empty_span(value: T) -> Self {
        Self::new(value, None)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value()
    }
}

impl<T: Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // write span
        if self.1.is_some() {
            write!(f, "({:?}) ", self.1.as_ref().unwrap())?;
        } else {
            write!(f, "(None) ")?;
        }

        // write value
        if f.alternate() {
            write!(f, "{:#?}", self.0)
        } else {
            write!(f, "{:?}", self.0)
        }
    }
}

impl<T> From<Spanned<T>> for Spanned<Opt<T>> {
    fn from(from: Spanned<T>) -> Self {
        Spanned::new(Ok(from.0), from.1)
    }
}

/// Utility function to create a [`Spanned`] from a value and some span
/// 
/// Designed to be used with [map_with_span](chumsky::Parser::map_with_span)
pub fn span<T>(value: T, span: Span) -> Spanned<T> {
    Spanned::spanned(value, span)
}

/// Utility function to create a [`Spanned`] from a value and no span
/// 
/// Designed to be used with [`map`](chumsky::Parser::map)
pub fn no_span<T>(value: T) -> Spanned<T> {
    Spanned::empty_span(value)
}




#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Opt<T> {
    Ok(T),
    Err,
}

impl<T> Opt<T> {
    pub fn is_ok(&self) -> bool {
        matches!(&self, &Ok(_))
    }

    pub fn is_err(&self) -> bool {
        matches!(&self, &Err)
    }
}

impl<T> Deref for Opt<T> {
    type Target = T;

    /// Dereferences the [`Opt`], returning the Ok value
    /// 
    /// # Panics
    /// 
    /// - If the [`Opt`] is an Err
    fn deref(&self) -> &Self::Target {
        match &self {
            Ok(x) => x,
            Err => panic!("Tried to dereference an empty Opt"),
        }
    }
}

/// Utility function to create an [`Ok`](Opt::Ok) [`Spanned<Opt<T>>`] from a value and span
/// 
/// Designed to be used with [`map_with_span`](chumsky::Parser::map_with_span)
pub fn ok_span<T>(value: T, span: Span) -> Spanned<Opt<T>> {
    self::span(Opt::Ok(value), span)
}

/// Utility function to create an [`Err`](Opt::Err) [`Spanned<Opt<T>>`] from a span
/// 
/// Designed to be used with any [`recovery`](chumsky::recovery) with a `fallback`
pub fn err_span<T>(span: Span) -> Spanned<Opt<T>> {
    self::span(Opt::Err, span)
}






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

