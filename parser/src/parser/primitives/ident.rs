use std::ops::Deref;

use crate::parser::prelude::*;

/// A struct that holds some extra information about a string
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Ident {
    pub(crate) id: String,
}

impl Ident {
    pub fn new(id: String) -> Ident {
        Ident{id}
    }
}

impl Deref for Ident {
    type Target = String;

    fn deref(&self) -> &String {
        &self.id
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

pub fn ident() -> impl Parser<Token, Ident, Error = Simple<Token>> {
    filter(|tok| matches!(tok, IDENTIFIER(_)))
        .map(|tok| tok.into())
}