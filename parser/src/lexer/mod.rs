// imports
use std::fmt;
use chumsky::{prelude::*};

use keyword::Keyword;
use operator::Operator;


// modules
pub mod reserved;
mod atom;
pub mod prelude;

#[cfg(test)]
mod tests;


// reexports
pub use Token::*;
pub use reserved::*;


// span type
use super::Span;


// - token type stuff -
#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Token {
    // true or false
    BOOLEAN(bool),

    // "" or ''
    STRING(String),

    // numbers
    INTEGER(usize),
    FLOAT(String),

    // identifier / keywords
    IDENTIFIER(String),
    KEYWORD(Keyword),

    // operators
    UNK_OPERATOR(String),
    OPERATOR{op: Operator, assignment: bool},

    // comments
    DOC_COMMENT{com: String, inner: bool},
}

impl From<usize> for Token {
    fn from(v: usize) -> Self {
        Self::INTEGER(v)
    }
}

impl Token {
    pub fn is_bool        (&self) -> bool { matches!(&self,       &BOOLEAN(_)) }
    pub fn is_string      (&self) -> bool { matches!(&self,        &STRING(_)) }
    pub fn is_int         (&self) -> bool { matches!(&self,       &INTEGER(_)) }
    pub fn is_float       (&self) -> bool { matches!(&self,         &FLOAT(_)) }
    pub fn is_ident       (&self) -> bool { matches!(&self,    &IDENTIFIER(_)) }
    pub fn is_keyword     (&self) -> bool { matches!(&self,       &KEYWORD(_)) }
    pub fn is_unk_op      (&self) -> bool { matches!(&self,  &UNK_OPERATOR(_)) }
    pub fn is_operator    (&self) -> bool { matches!(&self,     &OPERATOR{..}) }
    pub fn is_doc_comment (&self) -> bool { matches!(&self,  &DOC_COMMENT{..}) }

    pub fn take_string(self) -> String {
        match self {
            STRING(x) => x,
            FLOAT(x) => x,
            IDENTIFIER(x) => x,
            UNK_OPERATOR(x) => x,
            _ => panic!("Tried to take the string of a token that does not represent a string"),
        }
    }

    pub fn take_int(self) -> usize {
        match self {
            INTEGER(x) => x,
            _ => panic!("Tried to take the string of a token that does not represent a string"),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        // i don't know if there's a better way to do this
        match self {
            BOOLEAN(x) => write!(f, "{}", x),
            STRING(x) => write!(f, "{}", x),
            INTEGER(x) => write!(f, "{}", x),
            FLOAT(x) => write!(f, "{}", x),
            IDENTIFIER(x) => write!(f, "{}", x),
            KEYWORD(x) => write!(f, "{}", x),
            UNK_OPERATOR(x) => write!(f, "unk({})", x),
            OPERATOR{op, assignment} => {
                if *assignment { write!(f, "{}=", op) }
                else { write!(f, "{}", op) }
            },
            DOC_COMMENT{com, inner} => {
                if *inner { write!(f, "/*!{}*/", com) }
                else { write!(f, "/**{}*/", com) }
            },
        }
    }
}

mod util {
    use super::prelude::*;

    pub fn id(string: &str) -> Token {
        IDENTIFIER(string.to_string())
    }

    pub fn integer(number: usize) -> Token {
        INTEGER(number)
    }

    pub fn float(string: &str) -> Token {
        FLOAT(string.to_string())
    }

    pub fn string(string: &str) -> Token {
        STRING(string.to_string())
    }

    pub fn op(op: operator::Operator, assignment: bool) -> Token {
        OPERATOR{op, assignment}
    }

    pub fn unk_op(string: &str) -> Token {
        UNK_OPERATOR(string.to_string())
    }

    pub fn doc_in(string: &str) -> Token {
        DOC_COMMENT { com: string.to_string(), inner: true }
    }

    pub fn doc_out(string: &str) -> Token {
        DOC_COMMENT { com: string.to_string(), inner: false }
    }
}


/// Creates a lexer which outputs a vector of [`Token`]s connected to their `Span`s
/// 
/// # Examples:
/// ```
/// use parser::lexer::prelude::*;
/// 
/// let result = lexer::create().parse("as @e: say(\"hi\")").unwrap();
/// let result: Vec<Token> = result.into_iter().map(|x| x.0).collect();
/// 
/// assert_eq!(result, vec![
///     KW_AS, OP_AT, id("e"), OP_COLON,
///     id("say"), OP_LPARA, string("hi"), OP_RPARA,
/// ]);
/// ```
pub fn create() -> impl Parser<char, Vec<(Token, Span)>, Error = Simple<char>> {
    use atom::*;

    let token = 
        doc_comment().or(
            choice((
                string(),
                float(), integer(),
                identifier(), operator(),
            ))
        ).recover_with(skip_then_retry_until([]));

    token
        .map_with_span(|token, span| (token, span))
        .padded_by(comment().repeated())
        .padded().repeated()
        .then_ignore(end())

    // token
    //     .map_with_span(|token, span| (token, span))
    //     .padded().repeated()
    //     .separated_by(comment().repeated())
    //         .allow_leading().allow_trailing()
    //     .flatten()
    //     .then_ignore(end())
}
