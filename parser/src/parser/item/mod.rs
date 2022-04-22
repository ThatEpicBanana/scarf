use crate::prelude::*;

pub use attribute::inner_attribute;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Item {
    InnerAttribute(Opt<attribute::Attribute>),
    Error,
}

pub fn item() -> impl Parser<Token, Item, Error = Simple<Token>> {
    inner_attribute().map(Item::InnerAttribute)
}