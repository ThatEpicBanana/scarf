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
            map_ok_span(Attribute::new_inner(
                Path::from_offset_string(3, "doc"), //TODO: add tests for path parser
                map_span(vec![(string("doc comment"), 9..22)], 7..22)
            ), 2..23),
            // -- //! also doc comment
            map_ok_span(Attribute::inner_doc(
                map_span(vec![(doc_in(" also doc comment"), 27..49)], 27..49)
            ), 27..49),
            // -- #![thate:raycast.idk(pog, class)]
            map_ok_span(Attribute::new_inner(
                Path::from_offset_string(54, "thate:raycast.idk"), 
                map_span(vec![
                    (OP_LPARA, 71..72),
                        (id("pog"), 72..75),
                        (OP_COMM, 75..76),
                        (KW_CLASS, 77..82),
                    (OP_RPARA, 82..83),
                ], 71..83),
            ), 53..84),
            // -- //! doc comment
            // -- //! with lines
            map_ok_span(Attribute::inner_doc(
                map_span(vec![(doc_in(" doc comment\n with lines"), 88..119)], 88..119)
            ), 88..119),
        ], 
        HashMap::new()
    );
}

#[test]
fn patterns() {
    use pattern::{*, Pattern::*};
    use span as s;

    test_parser(include_str!("patterns.sf"), 
        parse!(SinglePattern)
            // // useful for debugging, but breaks parser and adds an error at the end
            // .map_with_span(ok_span)
            // .recover_with(skip_until([OP_SEMI], err_span))
            .separated_by(just(OP_SEMI)).allow_trailing()
            .then_ignore(end()),  
        vec![
            // real world
            s(15..208, DataPattern::compound(vec![
                // id
                s(22..24, CompoundPatternField::simple( 
                    s(22..24, Ident::from("id")), 
                    s(24..24, IdentifierInfo::empty())
                )),
                // Slot: @ 1
                s(31..40, CompoundPatternField::pattern(
                    s(31..35, Ident::from("Slot")), 
                    s(37..40, Bound(s(39..40, Expression::Temp)))
                )),
                // tag: {}
                s(47..205, CompoundPatternField::pattern(
                    s(47..50, Ident::from("tag")), 
                    s(52..205, DataPattern::compound(vec![
                        // Enchantments: []
                        s(63..198, CompoundPatternField::pattern(
                            s(63..75, Ident::from("Enchantments")), 
                            s(77..198, DataPattern::list(vec![
                                // {}: _
                                s(92..169, Pattern::Data{
                                    // {}
                                    pat: s(92..166, DataPattern::compound(vec![
                                        // id: ench_id
                                        s(111..122, CompoundPatternField::pattern(
                                            s(111..113, Ident::from("id")), 
                                            // ench_id
                                            s(115..122, Pattern::id( 
                                                s(115..122, Ident::from("ench_id")), 
                                                s(122..122, IdentifierInfo::empty()) 
                                            ))
                                        )),
                                        // lvl as int
                                        span(141..151, CompoundPatternField::simple( 
                                            span(141..144, Ident::from("lvl")), 
                                            span(145..151, IdentifierInfo::new(Some(s(148..151, Type::Temp)), None, None))
                                        )),
                                    ])),
                                    // : _
                                    typ: Some(s(168..169, Type::Temp)),
                                }).into(),
                                // ...
                                s(184..187, Pattern::Rest { typ: None, id: None }).into()
                            ]))
                        ))
                    ]))
                ))
            ])).into(),
            // error in compound check
            s(241..308, DataPattern::compound(vec![
                s(248..305, CompoundPatternField::pattern(
                    s(248..252, Ident::from("test")), 
                    s(254..305, DataPattern::compound(vec![
                        s(265..298, CompoundPatternField::pattern(
                            s(265..269, Ident::from("test")), 
                            s(271..298, DataPattern::Compound(Err))
                        )),
                    ]))
                )),
            ])).into(),
            // enum-ident error check
            s(340..353, Pattern::Enum {
                path: s(340..350, GenericPath::new(Path::from_offset_string(340, "enumm.path"), None)),
                pat: s(350..353, DataPattern::Tuple(Err)),
                typ: None,
            }).into(),
            // semi-full check
            s(378..549, Pattern::Enum {
                // this.name<generics>()
                path: s(378..397, GenericPath::new(
                    Path::from_offset_string(378, "this.name"), 
                    Some(GenericArguments::from_offset_string(387, "<generics>"))
                )),
                pat: s(397..549, DataPattern::tuple(vec![
                    // _name: type @ 1
                    s(404..419, Pattern::id(
                        s(404..409, Ident::from("_name")),
                        s(409..419, IdentifierInfo::new(
                            Some(s(411..415, Type::Temp)), 
                            Some(s(418..419, Expression::Temp)), 
                            None
                        ))
                    )).into(),
                    // []
                    s(426..545, DataPattern::list(vec![
                        // _ @ 1
                        s(437..442, Pattern::id(
                            s(437..438, Ident::from("_")),
                            s(439..442, IdentifierInfo::new(
                                None, Some(s(441..442, Expression::Temp)), None
                            ))
                        )).into(),
                        // {}
                        s(453..538, DataPattern::compound(vec![
                            // foo @ 1
                            span(468..475, CompoundPatternField::simple( 
                                span(468..471, Ident::from("foo")), 
                                span(472..475, IdentifierInfo::new(None, Some(s(474..475, Expression::Temp)), None))
                            )),
                            // key as type
                            span(490..501, CompoundPatternField::simple( 
                                span(490..493, Ident::from("key")), 
                                span(494..501, IdentifierInfo::new(Some(s(497..501, Type::Temp)), None, None))
                            )),
                            // ...rest: _
                            span(516..526, CompoundPatternField::Rest { 
                                id: Some(s(519..523, Ident::from("rest"))), 
                                typ: Some(s(525..526, Type::Temp))
                            })
                        ])).into(),
                    ])).into()
                ])),
                typ: None,
            }).into(),
        ].into_iter().map(|x| Box::new(x)).collect(), 
        HashMap::from([
            (286..287, (SimpleReason::Unexpected, Some(OP_STAR))),
            (351..352, (SimpleReason::Unexpected, Some(OP_STAR))),
            (426..545, (SimpleReason::Custom("Lists must be completely consisted of the same type.".to_string()), None))
        ])
    );
}