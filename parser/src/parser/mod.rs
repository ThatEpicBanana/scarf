pub mod item;
pub mod expression;
pub mod pattern;
pub mod typ;

pub mod primitives;

pub mod prelude;
pub mod util;


use crate::parser::prelude::*;

#[parser_fn]
pub fn create() -> Vec<Item> {
    parse!(Item)
        .repeated()
        .then_ignore(end())
}
