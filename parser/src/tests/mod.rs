use crate::parser::prelude::*;

use chumsky::{Stream, error::SimpleReason};
use std::{fmt::Debug, collections::HashMap, cmp::Ordering};

use pretty_assertions::assert_eq;

fn test(source: &str, expected: Vec<Item>) {
    let super::ParserOutput{out, parser_errors, lexer_errors} = crate::parse(source);
    
    if lexer_errors.len() > 0 { panic!("Lexer Errors found: {:#?}\nRecovered Syntax Tree: {:#?}", lexer_errors, out); }
    if parser_errors.len() > 0 { panic!("Parser Errors found: {:#?}\nRecovered Syntax Tree: {:#?}", parser_errors, out); }

    assert_eq!(out.unwrap(), expected);
}

fn test_parser<T>(
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
        assert_eq!(out, expected);
    }

}

#[test]
fn inner_attributes() {
    use attribute::Attribute;

    test_parser(include_str!("inner_attributes.sf"), 
        attribute::inner_attribute().repeated().then_ignore(end()), 
        vec![
            // -- #![doc = "doc comment"]
            ok_span(Attribute::new_inner(
                Path::from_offset_string(3, "doc"), //TODO: add tests for path parser
                span(vec![(string("doc comment"), 9..22)], 7..22)
            ), 2..23),
            // -- //! also doc comment
            ok_span(Attribute::inner_doc(
                span(vec![(doc_in(" also doc comment"), 27..49)], 27..49)
            ), 27..49),
            // -- #![thate:raycast.idk(pog, class)]
            ok_span(Attribute::new_inner(
                Path::from_offset_string(54, "thate:raycast.idk"), 
                span(vec![
                    (OP_LPARA, 71..72),
                        (id("pog"), 72..75),
                        (OP_COMM, 75..76),
                        (KW_CLASS, 77..82),
                    (OP_RPARA, 82..83),
                ], 71..83),
            ), 53..84),
            // -- //! doc comment
            // -- //! with lines
            ok_span(Attribute::inner_doc(
                span(vec![(doc_in(" doc comment\n with lines"), 88..119)], 88..119)
            ), 88..119),
        ], 
        HashMap::new()
    );
}

#[test]
fn patterns() {
    test_parser(include_str!("patterns.sf"), 
        pattern::pattern()
            // // useful for debugging, but breaks parser and adds an error at the end
            // .map_with_span(ok_span)
            // .recover_with(skip_until([OP_SEMI], err_span))
            .separated_by(just(OP_SEMI)).allow_trailing()
            .then_ignore(end()),  
        vec![
            // real world

        ], 
        HashMap::from([
            (286..287, (SimpleReason::Unexpected, Some(OP_STAR))),
            (351..352, (SimpleReason::Unexpected, Some(OP_STAR))),
            (416..535, (SimpleReason::Custom("Lists must be completely consisted of the same type.".to_string()), None))
        ])
    );
}