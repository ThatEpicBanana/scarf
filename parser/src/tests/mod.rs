use crate::prelude::*;
use pretty_assertions::assert_eq;

fn test(source: &str, expected: Vec<Item>) {
    let super::ParserOutput{out, parser_errors, lexer_errors} = crate::parse(source);
    
    if parser_errors.len() > 0 { panic!("Parser Errors found: {:?}", parser_errors); }
    if lexer_errors.len() > 0 { panic!("Lexer Errors found: {:?}", lexer_errors); }

    assert_eq!(out.unwrap(), expected);
}

#[test]
fn inner_attributes() {
    use attribute::Attribute;
    use macros::{TokenStream, TokenGroup, Delimiter};

    test(include_str!("inner_attributes.sf"), vec![
        Item::InnerAttribute(Ok(Attribute::inner(
            Path::from("doc"), 
            TokenStream::single(string("doc comment").into())
        ))),
        Item::InnerAttribute(Ok(Attribute::inner(
            Path::from(vec!["doc".into()]), 
            TokenStream::single(doc_in(" also doc comment").into())
        ))),
        Item::InnerAttribute(Ok(Attribute::inner(
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
                    Delimiter::Parantheses
                ).into()
            )
        )))
    ])
}