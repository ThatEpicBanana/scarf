use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Temp,
}

pub fn typ() -> impl Parser<Token, S<Type>, Error = Simple<Token>> {
    any().to(Type::Temp)
        .labelled("type")
        .map_with_span(span)
}