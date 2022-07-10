use crate::parser::prelude::*;

pub type TokenStream = Vec<(Token, Span)>;

fn single_spanned_vector(tok: Token, span: Span) -> TokenStream {
    vec![(tok, span)]
}

#[parser_fn]
fn until_single(#[no_convert] until: Token) -> TokenStream {
    none_of([OP_LPARA, OP_LCURLY, OP_LSQUARE, until])
        .map_with_span(single_spanned_vector)
}

#[parser_fn]
fn until_inner(#[no_convert] until: Token, any_group: TokenStream) -> TokenStream {
    any_group.or(until_single(until.clone()))
        .repeated().flatten()
        .chain(just(until).map_with_span(single_spanned_vector))
}



/// Parses a [`TokenStream`] surrounded by any delimiter, while checking for nested delimiters
/// 
/// > **Note:** The output [`TokenStream`] is not automatically spanned.
/// > 
/// > If you want to turn it into [`Spanned`], then call [`map_with_span(`](chumsky::Parser::map_with_span) [`span`] [`)`](chumsky::Parser::map_with_span)
#[parser_fn]
pub fn any_group() -> TokenStream {
    recursive(|group|
        choice((
            just(OP_LPARA).map_with_span(single_spanned_vector)
                .chain(until_inner(OP_RPARA, group.clone())),
            just(OP_LCURLY).map_with_span(single_spanned_vector)
                .chain(until_inner(OP_RCURLY, group.clone())),
            just(OP_LSQUARE).map_with_span(single_spanned_vector)
                .chain(until_inner(OP_RSQUARE, group.clone())),
        ))
    )
}

/// Parses a [`TokenStream`] until a given [`Token`], while checking for nested delimiters
/// 
/// > **Note:** The output [`TokenStream`] is not automatically spanned. 
/// >
/// > If you want to turn it into [`Spanned`], then call [`map_with_span(`](chumsky::Parser::map_with_span) [`span`] [`)`](chumsky::Parser::map_with_span)
#[parser_fn]
pub fn token_stream_until(#[no_convert] until: Token) -> TokenStream {
    any_group().or(until_single(until))
        .repeated().flatten()
}
