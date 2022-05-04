use crate::prelude::*;
use macros::*;

use path::Path;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub inner: bool,
    pub token_stream: TokenStream,
    pub path: Path,
}

impl Attribute {
    /// Constructs an outer attribute of a path and a token stream
    pub fn outer(path: Path, token_stream: TokenStream) -> Attribute {
        Attribute { inner: false, token_stream, path }
    }

    /// Constructs an inner attribute of a path and a token stream
    pub fn inner(path: Path, token_stream: TokenStream) -> Attribute {
        Attribute { inner: true, token_stream, path }
    }
}

fn doc_comment(inn: bool) -> impl Parser<Token, Attribute, Error = Simple<Token>> {
    filter(move |tok: &Token| 
        matches!(tok, 
            DOC_COMMENT { com: _, inner } 
                if inner == &inn.clone()
        )
    ).map(move |tok| 
        Attribute { 
            inner: inn, 
            path: "doc".into(),
            token_stream: vec![
                tok
            ].into()
        } 
    )
}

fn attribute_inner() -> impl Parser<Token, (Path, TokenStream), Error = Simple<Token>> {
    path::path()
    .then( // token stream
        ( // just a token group converted into a stream ex: `("tokens")`
            token_group().map(|group| TokenStream::single(group.into()))
        ).or(just(OP_EQUAL) // or an equal sign and a token stream until the end ex: `= "tokens"`
            .ignore_then(token_stream_until(OP_RSQUARE))
        ).then_ignore(just(OP_RSQUARE)) // then throw out the ending bracket
    )
}

pub fn outer_attribute() -> impl Parser<Token, Opt<Attribute>, Error = Simple<Token>> {
    just(OP_HASH)
    // make sure it's not inner to make sure recover doesn't mess up
    .then_ignore(none_of(OP_EXCLAMATION).rewind()) 
    // rest of attribute
    .ignore_then(
        just(OP_LSQUARE)
        .ignore_then(attribute_inner())
        .map(|(path, token_stream)| Ok(Attribute::outer(path, token_stream)))
        .recover_with(nested_delimiters(
            OP_LSQUARE, OP_RSQUARE, 
            [(OP_LCURLY, OP_RCURLY), (OP_LPARA, OP_RPARA), (OP_LANGLE, OP_RANGLE)],
            |range| Err(range)
        ))
    ).or(doc_comment(false).map(|attr| Ok(attr)))
}

pub fn inner_attribute() -> impl Parser<Token, Opt<Attribute>, Error = Simple<Token>> {
    just([OP_HASH, OP_EXCLAMATION]).ignore_then(
        just(OP_LSQUARE)
        .ignore_then(attribute_inner())
        .map(|(path, token_stream)| Ok(Attribute::inner(path, token_stream)))
        .recover_with(nested_delimiters(
            OP_LSQUARE, OP_RSQUARE, 
            [(OP_LCURLY, OP_RCURLY), (OP_LPARA, OP_RPARA), (OP_LANGLE, OP_RANGLE)],
            |range| Err(range)
        ))
    ).or(doc_comment(true).map(|attr| Ok(attr)))
}