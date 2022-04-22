use super::super::Token::{self, *};
use chumsky::{prelude::*};

pub fn string() -> impl Parser<char, Token, Error = Simple<char>> {
    //TODO: Multiline Strings
    let string_escape = just('\\').ignore_then(
            just('\\')
        .or(just('\''))
        .or(just('"'))
        .or(just('n').to('\n')) // newline
        .or(just('r').to('\r')) // carraige return
        .or(just('t').to('\t')) // tab
        .or(just('u').ignore_then( // stolen from json example
            filter(|c: &char| c.is_digit(16))
            .repeated().exactly(4)
            .collect::<String>()
            .validate(|digits, span, emit| {
                char::from_u32(u32::from_str_radix(&digits, 16).unwrap()) // convert digits to u32 and then char
                .unwrap_or_else(|| { // if converting to char failed, print error
                    emit(Simple::custom(span, "invalid unicode character"));
                    '\u{FFFD}' // unicode replacement character
                })
            })
        ))
    );


    let quote_string = just('\'') // start with ' or "
        .ignore_then(
            filter(|c: &char| *c != '\'') // followed by any characters that aren't a quote
            .or(string_escape) // while checking for escapes
        .repeated()) // repeat
        .then_ignore(just('\'')); // then ignore the ending quote

    let double_quote_string = just('"') // start with ' or "
        .ignore_then(
            filter(|c: &char| *c != '"') // followed by any characters that aren't a quote
            .or(string_escape) // while checking for escapes
        .repeated()) // repeat
        .then_ignore(just('"')); // then ignore the ending quote


    quote_string.or(double_quote_string)
        .collect::<String>() // collect chars to a string
        .map(STRING) // and convert to a token (as an enum is just a function)
}