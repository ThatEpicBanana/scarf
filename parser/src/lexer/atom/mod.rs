mod string;
pub use string::string;

mod number;
pub use number::integer;
pub use number::float;

mod comment;
pub use comment::*;

use chumsky::prelude::*;
use crate::lexer::prelude::*;

// - reserved stuff -
pub fn identifier() -> impl Parser<char, Token, Error = Simple<char>> {
    text::ident::<char, _>()
        .map(|string| {
            if let Some(identifier) = keyword::keywords.get(&string) {
                KEYWORD(*identifier)
            } else {
                match string.as_str() {
                    "true" => BOOLEAN(true),
                    "false" => BOOLEAN(false),
                    _ => IDENTIFIER(string),
                }
            }
        })
}


pub fn operator() -> impl Parser<char, Token, Error = Simple<char>> {
    choice((
            operator::any_match::top() 
        .or(operator::any_match::bottom())
        .or(operator::any_match::grouping())
            .map(|opr| op(opr, false)),
        operator::any_match::assign()
            .then(just('=').or_not())
            .map(|(op, assign)| OPERATOR{op, assignment: assign.is_some()}),
        filter::<char, _, _>(|c| c.is_ascii_punctuation())
            .repeated().at_least(1).collect()
            .map(|opr| UNK_OPERATOR(opr))
    ))
}