use crate::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenStream {
    trees: Vec<TokenTree>,
}

impl TokenStream {
    pub fn empty() -> TokenStream {
        TokenStream { trees: Vec::new() }
    }

    pub fn new(trees: Vec<TokenTree>) -> TokenStream {
        TokenStream{ trees }
    }

    pub fn single(tree: TokenTree) -> TokenStream {
        TokenStream { trees: vec![tree] }
    }
}

impl From<Vec<Token>> for TokenStream {
    fn from(tokens: Vec<Token>) -> TokenStream {
        tokens
            .into_iter()
            .map(|tok| TokenTree::Token(tok))
            .collect::<Vec<_>>()
            .into()
    }
}

impl From<Vec<TokenTree>> for TokenStream {
    fn from(tokens: Vec<TokenTree>) -> TokenStream {
        TokenStream::new(tokens)
    }
}



#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenTree {
    Group(Opt<TokenGroup>),
    Token(Token),
}

impl From<Opt<TokenGroup>> for TokenTree {
    fn from(group: Opt<TokenGroup>) -> TokenTree {
        TokenTree::Group(group)
    }
}

impl From<TokenGroup> for TokenTree {
    fn from(group: TokenGroup) -> TokenTree {
        Ok(group).into()
    }
}

impl From<Token> for TokenTree {
    fn from(token: Token) -> TokenTree {
        TokenTree::Token(token)
    }
}



#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TokenGroup {
    stream: Box<TokenStream>,
    delimiter: Delimiter,
}

impl TokenGroup {
    pub fn new(stream: TokenStream, delimiter: Delimiter) -> TokenGroup {
        TokenGroup { stream: Box::new(stream), delimiter }
    }

    pub fn get_owned_stream(self) -> TokenStream {
        *self.stream
    }
}



#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Delimiter {
    Curly,
    Parantheses,
    Square,
    // Angle,
}





fn token_stream(any_group: impl Parser<Token, Opt<TokenGroup>, Error = Simple<Token>>, until: Token) -> impl Parser<Token, TokenStream, Error = Simple<Token>> {
    any_group.map(|group| TokenTree::Group(group))
    .or(filter(move |tok| *tok != until).map(|tok| TokenTree::Token(tok)))
        .repeated()
        .map(|trees| TokenStream::new(trees))
}


fn token_group_inner(
    any_group: impl Parser<Token, Opt<TokenGroup>, Error = Simple<Token>>,
    left: Token, right: Token, 
    rest: [(Token, Token); 3],
    delimiter: Delimiter
) -> impl Parser<Token, Opt<TokenGroup>, Error = Simple<Token>> {

    token_stream(any_group, right.clone()).delimited_by(just(left.clone()), just(right.clone()))
        .map(move |stream| Ok(TokenGroup::new(stream, delimiter)))
        .recover_with(nested_delimiters(
            left, right, 
            rest,
            |span| Err(span)
        ))
}

/// Parses a token group of any [`Delimiter`]
pub fn token_group() -> impl Parser<Token, Opt<TokenGroup>, Error = Simple<Token>> {
    recursive(|group| choice((
        token_group_inner(
            group.clone(), 
            OP_LCURLY, OP_RCURLY, 
            [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE), (OP_LANGLE, OP_RANGLE)], 
            Delimiter::Curly
        ),
        token_group_inner(
            group.clone(), 
            OP_LPARA, OP_RPARA, 
            [(OP_LCURLY, OP_RCURLY), (OP_LSQUARE, OP_RSQUARE), (OP_LANGLE, OP_RANGLE)], 
            Delimiter::Parantheses
        ),
        token_group_inner(
            group.clone(), 
            OP_LSQUARE, OP_RSQUARE, 
            [(OP_LCURLY, OP_RCURLY), (OP_LPARA, OP_RPARA), (OP_LANGLE, OP_RANGLE)], 
            Delimiter::Square
        ),
        // token_group_inner(
        //     group, 
        //     OP_LANGLE, OP_RANGLE, 
        //     [(OP_LCURLY, OP_RCURLY), (OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE)], 
        //     Delimiter::Angle
        // ),
    )))
}


// /// Parses a token group of the given [`Delimiter`]
// pub fn token_group_of(delimiter: Delimiter) -> impl Parser<Token, Opt<TokenGroup>, Error = Simple<Token>> {
//     match delimiter {
//         Delimiter::Curly => token_group_inner(
//             token_group(), 
//             OP_LCURLY, OP_RCURLY, 
//             [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE), (OP_LANGLE, OP_RANGLE)], 
//             Delimiter::Curly
//         ),
//         Delimiter::Parantheses => token_group_inner(
//             token_group(), 
//             OP_LPARA, OP_RPARA, 
//             [(OP_LCURLY, OP_RCURLY), (OP_LSQUARE, OP_RSQUARE), (OP_LANGLE, OP_RANGLE)], 
//             Delimiter::Parantheses
//         ),
//         Delimiter::Square => token_group_inner(
//             token_group(), 
//             OP_LSQUARE, OP_RSQUARE, 
//             [(OP_LCURLY, OP_RCURLY), (OP_LPARA, OP_RPARA), (OP_LANGLE, OP_RANGLE)], 
//             Delimiter::Square
//         ),
//     }
// }

// outer facing token stream
/// Creates a token stream that takes in tokens or token groups until `end` is found
pub fn token_stream_until(end: Token) -> impl Parser<Token, TokenStream, Error = Simple<Token>> {
    token_stream(token_group(), end)
}