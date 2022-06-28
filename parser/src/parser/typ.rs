use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
    Temp,
}

#[parser_util(derive_parsable)]
impl Type {
    pub fn parser() -> S<Type> {
        any().to(Type::Temp)
            .labelled("type")
            .map_with_span(map_span)
    }
}
