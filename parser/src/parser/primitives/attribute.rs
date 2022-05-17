use crate::prelude::*;
use macros::*;

use path::Path;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum AttributeType {
    Path(S<Path>),
    DocComment,
}

/// A struct that holds an attribute
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Attribute {
    pub inner: bool,
    pub token_stream: S<TokenStream>,
    pub attr_type: AttributeType,
}

impl Attribute {
    /// Constructs an outer attribute of a path and a token stream
    pub fn new_outer(path: S<Path>, token_stream: S<TokenStream>) -> Attribute {
        Attribute { inner: false, token_stream, attr_type: AttributeType::Path(path) }
    }

    /// Constructs an inner attribute of a path and a token stream
    pub fn new_inner(path: S<Path>, token_stream: S<TokenStream>) -> Attribute {
        Attribute { inner: true, token_stream, attr_type: AttributeType::Path(path) }
    }

    pub fn inner_doc(token_stream: S<TokenStream>) -> Attribute {
        Attribute { inner: true, token_stream, attr_type: AttributeType::DocComment }
    }

    pub fn outer_doc(token_stream: S<TokenStream>) -> Attribute {
        Attribute { inner: false, token_stream, attr_type: AttributeType::DocComment }
    }

    pub fn inner(&self) -> bool {  self.inner   }
    pub fn outer(&self) -> bool { !self.inner() }
}

fn doc_comment(inn: bool) -> impl Parser<Token, S<Attribute>, Error = Simple<Token>> {
    filter(move |tok: &Token| 
        matches!(tok, 
            DOC_COMMENT { com: _, inner } 
                if inner == &inn.clone()
        )
    ).map_with_span(move |tok, spn: Span| map_span(
        // doc comments are spanned with and hold a stream with a span equal to its only token's span
        Attribute { 
            inner: inn, 
            attr_type: AttributeType::DocComment,
            token_stream: map_span(vec![(tok, spn.clone())], spn.clone()),
        },
        spn
    ))
}

fn attribute_inner() -> impl Parser<Token, (S<Path>, S<TokenStream>), Error = Simple<Token>> {
        parse!(Path)
    .then(
        any_group()
        .or(just(OP_EQUAL).ignore_then(token_stream_until(OP_RSQUARE)))
            .map_with_span(map_span)
        .then_ignore(just(OP_RSQUARE))
    )
}

pub fn outer_attribute() -> impl Parser<Token, S<Opt<Attribute>>, Error = Simple<Token>> {
    just(OP_HASH)
    // make sure it's not inner to make sure recover doesn't mess up
    .then_ignore(none_of(OP_EXCLAMATION).rewind()) 
    // rest of attribute
    .ignore_then(
        just(OP_LSQUARE)
        .ignore_then(attribute_inner())
        .map_with_span(|(path, token_stream), spn| 
            map_span(Ok(Attribute::new_outer(path, token_stream)), spn)
        ).recover_with(nested_delimiters(
            OP_LSQUARE, OP_RSQUARE, 
            [(OP_LCURLY, OP_RCURLY), (OP_LPARA, OP_RPARA)],
            err_span
        ))
    ).or(doc_comment(false).map(|attr| attr.into()))
}

pub fn inner_attribute() -> impl Parser<Token, S<Opt<Attribute>>, Error = Simple<Token>> {
    just([OP_HASH, OP_EXCLAMATION])
    .ignore_then(
        just(OP_LSQUARE)
        .ignore_then(attribute_inner())
        .map_with_span(|(path, token_stream), spn| 
            map_span(Ok(Attribute::new_inner(path, token_stream)), spn)
        ).recover_with(nested_delimiters(
            OP_LSQUARE, OP_RSQUARE, 
            [(OP_LCURLY, OP_RCURLY), (OP_LPARA, OP_RPARA)],
            err_span
        ))
    ).or(doc_comment(true).map(|attr| attr.into()))
}