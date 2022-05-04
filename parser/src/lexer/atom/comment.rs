use chumsky::prelude::*;
use crate::lexer::prelude::*;

fn until_end() -> impl Parser<char, (Vec<char>, ()), Error = Simple<char>> + Clone {
    take_until(
        just("\r\n")
        .or(just("\n"))
            .ignored()
        .or(end().rewind())
    )
}

fn until_end_collect() -> impl Parser<char, String, Error = Simple<char>> {
    until_end()
        .map(|(string, _)| string)
        .collect::<String>()
}

pub fn comment() -> impl Parser<char, (), Error = Simple<char>> + Clone {
    let single = just("//")
        .then(none_of("/!"))
        .then(until_end()).padded();

    let multi = just("/*").then(
        none_of("*!").ignored()
            .or(just("**").ignored())
        .then(recursive(|multi|
            take_until( // go until:
                just("/*") // another 
                    .then(multi.clone()) // account for the other
                    .then(multi) // account for the current one
                    .ignored()
                .or(just("*/") // or end
                    .ignored()
                ) 
            )
        ))
    );

    choice((
        just("/**/").ignored(),
        just("/***/").ignored(),
    )).or(choice((
        single.ignored(),
        multi.ignored(),
    ))).padded()
}

pub fn doc_comment() -> impl Parser<char, Token, Error = Simple<char>> {
    let single = just("//")
        .ignore_then( // check if inner or not
                just("!")
            .or(just("/"))
        ).then_with(move |inner| 
            until_end_collect().then( // save until end
                just("//".to_string() + inner) // check for repeats
                    .ignore_then(until_end_collect())
                    .repeated()
            ).foldl(|left, right| left + "\n" + &right) // concatenate repeats 
            .map(move |com| (com, inner)) // add inner back
        ).map(|(com, inner)| DOC_COMMENT{ com, inner: inner == "!" }); // turn into token

    let multi = just("/*")
        .ignore_then( // check if inner or not
            just("!").to(true)
            .or(just("*").to(false))
        ).then(take_until( // take until end while checking for escapes
            none_of("\\")
                .then_ignore(just("*/"))
        )).map(|(inner, (mut com, end))| 
            DOC_COMMENT{ // collect into token
                inner,
                com: { 
                    com.push(end); // add ending back to vector
                    com.into_iter()
                        .filter(|&c| c != '\r') // filter out carriage returns
                        .collect() // collect into string
                }
            } 
        ); 

    single.or(multi)
}