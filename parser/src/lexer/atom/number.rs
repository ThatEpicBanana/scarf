use super::super::Token::{self, *};
use chumsky::{prelude::*};

// helper function for concatenating strings
fn concat(a: String, b: String) -> String { a + &b }

fn delim_number() -> impl Parser<char, String, Error = Simple<char>> {
    let delim_number = text::int(10);

    delim_number
        .then( // base-10 delimited by '_'
            just('_').repeated()
            .ignore_then(delim_number)
            .repeated() 
        ).then_ignore(just('_').or_not()) // ignore trailing '_'
        .foldl(concat) // concatenate all strings
}

pub fn integer() -> impl Parser<char, Token, Error = Simple<char>> {
    delim_number()
        .try_map(|s, span| Ok(INTEGER(
            s.parse().map_err( // parse integer and return error on failiure
                |e| Simple::custom(span, format!("{}", e))
            )?
        ))) // convert to integer and save
}

pub fn float() -> impl Parser<char, Token, Error = Simple<char>> {
    delim_number()
        .then_ignore(just('.'))
        .then(delim_number())
        .try_map(|(whole, decimal), span| {
            let full = format!("{}.{}", whole, decimal);
            full.parse::<f64>().map_err( // check if float is valid
                |e| Simple::custom(span, format!("{}", e))
            )?; // return error if not
            Ok(FLOAT(full)) // if it is valid, return string
        })
}