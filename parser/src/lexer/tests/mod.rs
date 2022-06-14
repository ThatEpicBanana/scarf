
// use crate::lexer::{self, keyword, operator};
use chumsky::{prelude::*};

// use super::Token::{self, *};

use crate::lexer::prelude::*;

use pretty_assertions::assert_eq;

fn test(src: &str, expected: Vec<Token>) -> Result<(), Vec<Simple<char>>> {
    let result = lexer::create().parse_recovery(src);

    if result.1.len() == 0 {
        match result.0 {
            Some(x) => {
                let result: Vec<Token> = x.into_iter().map(|x| x.0).collect();

                assert_eq!(result, expected);
            },
            None => {
                return Err(result.1); // shouldn't be possible
            },
        }
    } else {
        return Err(result.1);
    }

    Ok(())
}

#[test]
fn general_test() -> Result<(), Vec<Simple<char>>> {
    test(include_str!("general_test.sf"), vec![
        BOOLEAN(true),

        string("string1"),
        string("string2"),

        INTEGER(65535),
        float("3.1415"),

        id("Raycaster"),
        KW_MOD,

        unk_op("\\+-"),
        OP_PLUS
    ])
}

#[test]
fn number_separation() -> Result<(), Vec<Simple<char>>> {
    test(include_str!("number_separation.sf"), vec![
        INTEGER(1024), INTEGER(65535), INTEGER(10), INTEGER(1234), INTEGER(6969420),
        float("3.14"), float("3.1415")
    ])
}

#[test]
fn real_world() -> Result<(), Vec<Simple<char>>> {
    test(include_str!("real_world_test.sf"), vec![
        KW_PUB, KW_FUNC, id("main"), OP_LPARA, OP_RPARA, OP_LCURLY,
            KW_AS, OP_AT, id("a"), OP_COLON,
                id("clear"), OP_LPARA, string("tnt"), id("id"), OP_RPARA, OP_SEMI,
        OP_RCURLY
    ])
}

#[test]
fn comments() -> Result<(), Vec<Simple<char>>> {
    test(include_str!("comments.sf"), vec![
        id("token"), id("here"), 
        id("these"), id("should"), id("though"), 
        doc_out(" this is a doc comment\n this should be concatenated"),
        doc_in(" inner comment"),
        doc_out("\nouter block comment\n\n\\*/ with escape\n\n"), 
        doc_in(" inner block comment ")
    ])
}

#[test]
fn add_one() -> Result<(), Vec<Simple<char>>> {
    test(include_str!("add_one.sf"), vec![
        doc_in(" crate about arithmetic idk"),

        doc_out(" Adds one to the given number"),

        kw!("pub"), kw!("func"), id("add_one"),
        op!("("), id("x"), op!(":"), id("int"), op!(")"),
        op!("=>"), id("x"), op!("+"), integer(1), op!(";"),
    ])
}

// #[test]
// fn unknown_token() {
//     // test("", vec![]);
// }
