use crate::parser::prelude::*;

pub use attribute::inner_attribute;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Temp,
}

#[derive_parsable]
impl Expression {
    pub fn parser() -> impl Parser<Token, S<Expression>, Error = Simple<Token>> + Clone {
        any().to(Expression::Temp)
            .labelled("expression")
            .map_with_span(map_span)
    }
}
