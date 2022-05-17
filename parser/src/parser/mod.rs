pub mod item;
pub mod expression;
pub mod pattern;
pub mod typ;

pub mod primitives;

pub mod prelude;

use std::fmt::{self, Debug};
use std::iter;
use std::ops::Deref;

/// A type representing a span of input text 
pub type Span = std::ops::Range<usize>;

/// A simple (smart pointer) struct that associates a type with a [`Span`]
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T>(Option<Span>, T);
/// A shorter alias for [`Spanned`]
pub type S<T> = Spanned<T>;

impl<T> Spanned<T> {
    /// Returns a clone of the held span
    pub fn span(&self) -> Option<Span> {
        self.0.clone()
    }

    /// Takes an owned value out of a span
    pub fn unspan     ( self) ->  T {  self.1 }
    /// References a value from a span
    pub fn unspan_ref (&self) -> &T { &self.1 }
    
    /// Creates a [`Spanned`] from a given value and optional span
    pub fn new(span: Option<Span>, value: T) -> Self {
        Spanned(span, value)
    }
    
    // man i'm getting jamais vu
    /// Creates a [`Spanned`] from a value and some span
    pub fn spanned(value: T, span: Span) -> Self {
        Self::new(Some(span), value)
    } 

    /// Creates a [`Spanned`] from a value with no span
    pub fn empty_span(value: T) -> Self {
        Self::new(None, value)
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.unspan_ref()
    }
}

impl<T: Debug> fmt::Debug for Spanned<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // write span
        if self.0.is_some() {
            write!(f, "({:?}) ", self.0.as_ref().unwrap())?;
        } else {
            write!(f, "(None) ")?;
        }

        // write value
        if f.alternate() {
            write!(f, "{:#?}", self.1)
        } else {
            write!(f, "{:?}", self.1)
        }
    }
}

impl<T> From<Spanned<T>> for Spanned<Opt<T>> {
    fn from(from: Spanned<T>) -> Self {
        Spanned::new(from.0, Ok(from.1), )
    }
}

/// Utility function to create a [`Spanned`] from a value and some span
/// 
/// Designed to be used with [map_with_span](chumsky::Parser::map_with_span)
pub fn map_span<T>(value: T, spn: Span) -> Spanned<T> {
    Spanned::spanned(value, spn)
}

/// Utility function to create a [`Spanned`] from a value and some span
/// 
/// Alternative to [`map_span`] for readability
pub fn span<T>(spn: Span, value: T) -> Spanned<T> {
    map_span(value, spn)
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

    pub fn unwrap(self) -> T { 
        match self {
            Ok(x) => x,
            Err => panic!("Tried to unwrap an Err!"),
        }
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
pub fn map_ok_span<T>(value: T, span: Span) -> Spanned<Opt<T>> {
    self::map_span(Opt::Ok(value), span)
}

/// Utility function to create a [`Spanned`] from a value and some span
/// 
/// Alternative to [`map_ok_span`] for readability
pub fn ok_span<T>(span: Span, value: T) -> Spanned<Opt<T>> {
    self::map_span(Opt::Ok(value), span)
}

/// Utility function to create an [`Err`](Opt::Err) [`Spanned<Opt<T>>`] from a span
/// 
/// Designed to be used with any [`recovery`](chumsky::recovery) with a `fallback`
pub fn err_span<T>(span: Span) -> Spanned<Opt<T>> {
    self::map_span(Opt::Err, span)
}

impl<T> Spanned<Opt<T>> {
    pub fn unwrap_span(self) -> T { self.unspan().unwrap() }
}


#[macro_export]
macro_rules! parse {
    ($x:ident) => {
        $x::parser()
    };
} use chumsky::Stream;
pub use parse;


/// Creates a string offset by `offset`
fn offset_string(offset: usize, string: &str) -> String {
    iter::repeat(' ').take(offset)   // add `offset` spaces
    .chain(string.chars()).collect() // to start of string
}

/// Parses a `string` with a given `parser`, passing it through the lexer first.
/// 
/// `name`: The name to call the output in error messages
fn lex_to_parse<O>(string: &str, parser: impl Parser<Token, O, Error = Simple<Token>>, name: &str) -> O
where 
    O: Clone + Debug + PartialEq + Eq + std::hash::Hash 
{
    let len = string.len();
    
    parser.parse(Stream::from_iter(len..len+1,
        crate::lexer::create().parse(
            string
        ).expect(format!("Failed to lex {}!", name).as_str()).into_iter()
    )).expect(format!("Failed to parse {}!", name).as_str())
}


use crate::parser::prelude::*;

pub fn create() -> impl Parser<Token, Vec<Item>, Error = Simple<Token>> {
    item::item()
        .repeated()
        .then_ignore(end())
}