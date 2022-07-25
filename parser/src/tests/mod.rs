use crate::parser::prelude::{*, pattern::SinglePattern};

use chumsky::Stream;
use std::{fmt::Debug, collections::HashMap, cmp::Ordering};

use pretty_assertions::assert_eq;

pub mod prelude {
    pub use crate::parser::prelude::*;
    pub use super::{test_parser, parser_test};

    pub use pretty_assertions::assert_eq;
    pub use indoc::indoc;

    pub use chumsky::error::SimpleReason;
    pub use std::collections::HashMap;
}

// TODO: make this return a result
pub fn test_parser<T>(
    input: &str,
    parser: parser!(T),
    expected: T,
    expected_errors: HashMap<Span, (ParserErrorReason, Option<Token>)>
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
        Ordering::Less  => panic!("Not enough parser errors found (expected {}, found {}): {:#?}\nRecovered Syntax Tree: {:#?}", expected_errors.len(), parser_errors.len(), parser_errors, out),
        Ordering::Greater => panic!("Too many parser errors found (expected {}, found {}): {:#?}\nRecovered Syntax Tree: {:#?}", expected_errors.len(), parser_errors.len(), parser_errors, out),
        _ => (),
    }

    for error in parser_errors.clone() {
        // check if error matches an expected error
        let res = if let Some((reason, tok)) = expected_errors.get(&error.span) {
            &error.found == tok && &error.reason == reason
        } else { false };

        // if it doesn't, panic
        if !res { panic!("Unexpected Parser Errors found: {:#?}\nRecovered Syntax Tree: {:#?}", parser_errors, out); }
    }

    if let Some(out) = out.clone() {
        assert_eq!(expected, out);
    }

}


/// A macro to make creating parser tests easier
///
/// # Examples
///
/// ```
/// # use parser::tests::prelude::*;
/// parser_test!(
///     parse!(Type),
///     (
///         "simple",
///         Type::parse("simple")
///     ),
///     (
///         "errors<*>",
///         // <-- expected output omitted -->
/// #       span(0..9, Type::path(
/// #           GenericArgPath {
/// #               path: Path::parse_offset(0, "errors"),
/// #               generics: Some(span(6..9, Err))
/// #           }
/// #       )).into(),
///         {
///             7..8 => Unexpected: op!("*")
///         }
///     )
/// );
/// ```
///
/// # Syntax
///
/// Represented in an unholy mixture of the minecraft wiki command syntax and rust macro syntax
///
/// ## Multiple Source
///
/// ```ignore
/// parser_test!(
///     <parser>,
///     (
///         <source>,
///         <expected_output>,
///        [<errors>]
///     ),*
/// )
/// ```
///
/// ## Single Source
///
/// ```ignore
/// parser_test!(
///     <parser>,
///     <source>,
///     <expected_output>,
///    [<errors>]
/// )
/// ```
///
/// ## Errors
///
/// ### Custom
/// ```ignore
/// {
///     <span> => Unexpected: <token>,
///     <span> => Reason: <reason>
/// }
/// ```
///
/// ### Normal
///
/// mirrors [`test_parser`]
/// ```ignore
/// [
///     (<span>, (<reason>, <unexpected_token>))
/// ]
/// ```
///
#[macro_export]
macro_rules! parser_test {
    // multiple source, single parser
    (
        $parser:expr,
        $( (
            $source:expr,
            $output:expr $(,
            $errors:tt )?
        ) ),*
    ) => {
        $(
            parser_test!(
                $parser,
                $source,
                $output $(,
                $errors
                )?
            );
        )*
    };
    // single source - no errors
    // quick escape for performance
    (
        $parser:expr,
        $source:expr,
        $output:expr
    ) => {
        test_parser($source, $parser, $output, HashMap::new())
    };
    // single source - custom error syntax
    (
        $parser:expr,
        $source:expr,
        $output:expr $(,
        {
            // 1..2 => Unexpected: op!("*")
            // 1..2 => Reason: PatternListSameType(array)
            $($span:expr => $type:ident : $expr:expr),*
        })?
    ) => {
        parser_test!(
            $parser,
            $source,
            $output $(,
            [
                $(
                    ($span, $crate::parser_test_error!($type: $expr))
                ),*
            ])?
        )
    };
    // single source - normal error syntax
    (
        $parser:expr,
        $source:expr,
        $output:expr $(,
        [
            $(
                // (1..2, (Unexpected,                 Some(op!("*"))))
                // (1..2, (PatternListSameType(array), None))
                ($span:expr, $tuple:expr)
            ),*
        ])?
    ) => {
        {
            #[allow(unused_imports)]
            use $crate::error::parser::ParserErrorReason::*;
            test_parser($source, $parser, $output, $crate::parser_test_errors!(
                $(
                    $(
                        ($span, $tuple)
                    ),*
                )?
            ))
        }
    };
} pub use parser_test;

#[macro_export]
macro_rules! parser_test_error {
    (Unexpected: $token:expr) => {
        (ParserErrorReason::Unexpected, Some($token))
    };
    (Reason: $expr:expr) => {
        ($expr, None)
    }
}

#[macro_export]
macro_rules! parser_test_errors {
    () => {
        HashMap::new()
    };
    ($($expr:expr),*) => {
        HashMap::from([
            $($expr),*
        ])
    }
}



#[test]
fn doctest() {
    use crate::tests::prelude::*;
    parser_test!(
        parse!(Type),
        (
            "simple",
            Type::parse("simple")
        ),
        (
            "errors<*>",
            span(0..9, Type::path(
                GenericArgPath {
                    path: Path::parse_offset(0, "errors"),
                    generics: Some(span(6..9, Err))
                }
            )).into(),
            {
                7..8 => Unexpected: op!("*")
            }
        )
    );
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
