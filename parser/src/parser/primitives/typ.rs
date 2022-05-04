use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Temp,
}

pub fn typ() -> impl Parser<Token, Type, Error = Simple<Token>> {
    any().to(Type::Temp)
}