use crate::parser::prelude::*;

use chumsky::Stream;
use std::{fmt::Debug, collections::HashMap, cmp::Ordering};

use pretty_assertions::assert_eq;

fn test(source: &str, expected: Vec<Item>) {
    let super::ParserOutput{out, parser_errors, lexer_errors} = crate::parse_verbose(source);
    
    if lexer_errors.len() > 0 { panic!("Lexer Errors found: {:#?}\nRecovered Syntax Tree: {:#?}", lexer_errors, out); }
    if parser_errors.len() > 0 { panic!("Parser Errors found: {:#?}\nRecovered Syntax Tree: {:#?}", parser_errors, out); }

    assert_eq!(out.unwrap(), expected);
}

fn test_parser<T>(parser: impl Parser<Token, Vec<T>, Error = Simple<Token>>, input: &str, expected: Vec<T>, expected_errors: HashMap<Span, Option<Token>>) 
where T: PartialEq + Eq + Debug + Clone {
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
        Ordering::Less => panic!("Not Enough Parser Errors Found (Expected {}, found {}): {:#?}\nRecovered Syntax Tree: {:#?}", expected_errors.len(), parser_errors.len(), parser_errors, out),
        Ordering::Greater => panic!("Too Many Parser Errors Found (Expected {}, found {}): {:#?}\nRecovered Syntax Tree: {:#?}", expected_errors.len(), parser_errors.len(), parser_errors, out),
        _ => (),
    }

    for error in parser_errors.clone() {
        // check if error matches an expected error
        let res = if let Some(tok) = expected_errors.get(&error.span()) {
            error.found() == tok.as_ref()
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
    use macros::{TokenStream, TokenGroup, Delimiter};

    test(include_str!("inner_attributes.sf"), vec![
        Item::simple(ItemVariant::InnerAttribute(Ok(Attribute::inner(
            Path::from("doc"), 
            TokenStream::single(string("doc comment").into())
        )))),
        Item::simple(ItemVariant::InnerAttribute(Ok(Attribute::inner(
            Path::from(vec!["doc".into()]), 
            TokenStream::single(doc_in(" also doc comment").into())
        )))),
        Item::simple(ItemVariant::InnerAttribute(Ok(Attribute::inner(
            Path::from(vec![
                "thate".into(),
                "raycast".into(),
                "idk".into()
            ]),
            TokenStream::single(
                TokenGroup::new(
                    TokenStream::from(vec![
                        id("pog"),
                        OP_COMM,
                        KW_CLASS
                    ]),
                    Delimiter::Parentheses
                ).into()
            )
        )))),
    ])
}

#[test]
fn patterns() {
    // test_parser(pattern::test().repeated().then_ignore(end()), include_str!("patterns.sf"), vec![]);
    test_parser(pattern::pattern().repeated().then_ignore(end()), include_str!("patterns.sf"), 
        vec![
            // omitted because of soon refactor
        ], 
        HashMap::from([
            (282..283, Some(OP_STAR)),
            (346..347, Some(OP_STAR)),
        ])
    );
}