pub mod item;
pub mod expression;
pub mod pattern;
pub mod typ;

pub mod primitives;

pub mod prelude;

use std::fmt::{self, Debug};
use std::ops::Deref;

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

    /// Takes an owned value out of a span
    pub fn unspan     ( self) ->  T {  self.0 }
    /// References a value from a span
    pub fn unspan_ref (&self) -> &T { &self.0 }
    
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
        &self.0
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
pub fn ok_span<T>(value: T, span: Span) -> Spanned<Opt<T>> {
    self::span(Opt::Ok(value), span)
}

/// Utility function to create an [`Err`](Opt::Err) [`Spanned<Opt<T>>`] from a span
/// 
/// Designed to be used with any [`recovery`](chumsky::recovery) with a `fallback`
pub fn err_span<T>(span: Span) -> Spanned<Opt<T>> {
    self::span(Opt::Err, span)
}

impl<T> Spanned<Opt<T>> {
    pub fn unwrap_span(self) -> T { self.unspan().unwrap() }
}


use crate::parser::prelude::*;


pub fn create() -> impl Parser<Token, Vec<Item>, Error = Simple<Token>> {
    item::item()
        .repeated()
        .then_ignore(end())
}