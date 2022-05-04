pub mod ident;
pub mod path;
pub mod macros;
pub mod pattern;
pub mod typ;

use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Visibility {
    Prv,
    Pub,
    PubPath(Opt<Path>),
}

pub fn visibility() -> impl Parser<Token, Visibility, Error = Simple<Token>> {
    just(KW_PRV).to(Visibility::Prv).or(
    just(KW_PUB).ignore_then(
        // check if there's a path and rewind for recovery
        just(OP_LPARA).rewind().ignore_then(
            path::path()
                .delimited_by(just(OP_LPARA), just(OP_RPARA))
                .map(Ok)
                .recover_with(nested_delimiters(OP_LPARA, OP_RPARA, [(OP_LCURLY, OP_RCURLY)], Err))
        ).or_not() // if there's no path then there's no path
    ).map(|path| 
        match path {
            Some(path) => Visibility::PubPath(path),
            None => Visibility::Pub,
        }
    ))
}