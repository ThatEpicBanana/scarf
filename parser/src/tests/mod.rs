use crate::parser::prelude::{*, pattern::SinglePattern};

use chumsky::{Stream, error::SimpleReason};
use std::{fmt::Debug, collections::HashMap, cmp::Ordering};

use pretty_assertions::assert_eq;

pub mod prelude {
    pub use crate::parser::prelude::*;
    pub use super::test_parser;

    pub use pretty_assertions::assert_eq;
    pub use indoc::indoc;

    pub use std::collections::HashMap;
}

pub fn test_parser<T>(
    input: &str, 
    parser: impl Parser<Token, T, Error = Simple<Token>>,
    expected: T,
    expected_errors: HashMap<Span, (SimpleReason<Token, Span>, Option<Token>)>
) where T: PartialEq + Eq + Debug + Clone {
    let len = input.len();

    // try lexing
    let (out, lexer_errors) = 
        crate::lexer::create().parse_recovery_verbose(input);

    // println!("Token Tree: {:#?}", out);

    // check output of lexer
    let (out, parser_errors) = if let Some(out) = out {
        // if lexer succeeds, try parsing
        parser.parse_recovery_verbose(Stream::from_iter(len..len + 1, out.into_iter()))
    } else {
        // if lexer fails, output lexer errors
        panic!("Lexer Errors found: {:#?}\nRecovered Syntax Tree: {:#?}", lexer_errors, out);
    };

    match parser_errors.len().cmp(&expected_errors.len()) {
        Ordering::Less => panic!("Not enough parser errors found (expected {}, found {}): {:#?}\nRecovered Syntax Tree: {:#?}", expected_errors.len(), parser_errors.len(), parser_errors, out),
        Ordering::Greater => panic!("Too many parser errors found (expected {}, found {}): {:#?}\nRecovered Syntax Tree: {:#?}", expected_errors.len(), parser_errors.len(), parser_errors, out),
        _ => (),
    }

    for error in parser_errors.clone() {
        // check if error matches an expected error
        let res = if let Some((reason, tok)) = expected_errors.get(&error.span()) {
            error.found() == tok.as_ref() && error.reason() == reason
        } else { false };

        // if it doesn't, panic
        if !res { panic!("Unexpected Parser Errors found: {:#?}\nRecovered Syntax Tree: {:#?}", parser_errors, out); }
    }

    if let Some(out) = out.clone() {
        assert_eq!(expected, out);
    }

}


#[test]
fn pattern_let() {
    use pattern::{Pattern, IdentifierInfo};

    test_parser(include_str!("pattern_let.sf"), 
        just(KW_LET)
            .ignore_then(SinglePattern::parser_no_default(parse!(Expression)))
        .then_ignore(just(OP_EQUAL))
            .then(parse!(Expression))
                .separated_by(just(OP_SEMI)).allow_trailing()
                .then_ignore(end()),
        vec![
            (
                span(4..5, Pattern::Identifier { 
                    id: span(4..5, "x".into()),
                    info: span(6..5, IdentifierInfo::empty()) 
                }).into(),
                span(8..10, Expression::Temp)
            )
        ],
        HashMap::from([])
    );
}
