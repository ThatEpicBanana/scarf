pub mod item;
pub mod expression;
pub mod pattern;
pub mod typ;

pub mod primitives;

pub mod prelude;
pub mod util;


use crate::parser::prelude::*;

pub fn create() -> impl Parser<Token, Vec<Item>, Error = Simple<Token>> {
    item::item()
        .repeated()
        .then_ignore(end())
}