use std::{ops::Deref, fmt};

use crate::parser::prelude::*;

/// A struct that holds some extra information about a string
#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Ident(String);

impl Ident {
    pub fn new(id: String) -> Ident {
        Ident(id)
    }
}

impl fmt::Debug for Ident {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "\"{}\"id", &self.0)
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &String {
        &self.0
    }
}

impl From<Token> for Ident {
    fn from(tok: Token) -> Ident {
        if let IDENTIFIER(idt) = tok {
            Ident::new(idt)
        } else {
            panic!("Tried to turn a non-identifier token into an Ident!")
        }
    }
}

impl From<&str> for Ident {
    fn from(string: &str) -> Ident {
        Ident::new(string.to_string())
    }
}

/// Parses a single [`IDENTIFIER`] into an [`Ident`]
/// 
/// Automatically [`Spanned`].
pub fn ident() -> impl Parser<Token, S<Ident>, Error = Simple<Token>> {
    filter(|tok| matches!(tok, IDENTIFIER(_)))
        .labelled("ident")
        .map(|tok| tok.into())
        .map_with_span(span)
}