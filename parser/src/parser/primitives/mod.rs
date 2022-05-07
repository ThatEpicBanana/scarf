pub mod ident;
pub mod path;
pub mod macros;
pub mod attribute;

use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Visibility {
    Prv,
    Pub,
    PubPath(S<Opt<S<Path>>>),
}

pub fn visibility() -> impl Parser<Token, S<Visibility>, Error = Simple<Token>> {
    just(KW_PRV).to(Visibility::Prv)
    .or(just(KW_PUB).ignore_then(
            // check if there's a path and rewind for recovery
            just(OP_LPARA).rewind().ignore_then(
                path::path()
                    .delimited_by(just(OP_LPARA), just(OP_RPARA))
                    .map_with_span(ok_span).recover_with(nested_delimiters(OP_LPARA, OP_RPARA, [(OP_LCURLY, OP_RCURLY)], err_span))
            ).or_not() // if there's no path then there's no path
        ).map(|path| 
            match path {
                Some(path) => Visibility::PubPath(path),
                None => Visibility::Pub,
            }
        ))
    .map_with_span(span)
}