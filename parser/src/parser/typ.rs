use ::macros::parser_util;

use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Temp,
}

#[parser_util]
impl Type {
    pub fn parser() -> impl Parser<Token, S<Type>, Error = Simple<Token>> {                
        any().to(Type::Temp)
            .labelled("type")
            .map_with_span(map_span)
    }
}