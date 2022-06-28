use crate::parser::prelude::*;

pub use attribute::Attribute;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expression {
    Temp,
}

#[parser_util(derive_parsable)]
impl Expression {
    pub fn parser() -> S<Expression> {
        any().to(Expression::Temp)
            .labelled("expression")
            .map_with_span(map_span)
    }
}
