use crate::parser::prelude::*;

pub use attribute::inner_attribute;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Temp,
}

pub fn expression() -> impl Parser<Token, S<Expression>, Error = Simple<Token>> {
    any().to(Expression::Temp)
        .labelled("expression")
        .map_with_span(map_span)
}