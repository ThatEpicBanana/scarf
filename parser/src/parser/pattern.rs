use crate::{parser::prelude::*, parse};

use super::prelude::primitives::path::IndexedPath;

// TODO: make it so bounds use patterns ( and @@ has the old functionality by default )

// simple alias
macro_rules! SPatParser {
    ($x:lifetime) => { impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone + $x };
    (           ) => { impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone      }
}

// simple alias
macro_rules! ExprParser {
    ($x:lifetime) => { impl Parser<Token,     S<Expression>,     Error = Simple<Token>> + Clone + $x };
    (           ) => { impl Parser<Token,     S<Expression>,     Error = Simple<Token>> + Clone      };
}



#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MultiPattern {
    pub patterns: Vec<Box<S<SinglePattern>>>,
}

#[derive_parsable]
impl MultiPattern {
    pub fn new(patterns: Vec<Box<S<SinglePattern>>>) -> MultiPattern {
        MultiPattern { patterns }
    }

    pub fn parser() -> impl Parser<Token, MultiPattern, Error = Simple<Token>> {
        parse!(SinglePattern)
            .separated_by(just(OP_BAR))
            .map(MultiPattern::new).labelled("multi pattern")
    }
}




#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SinglePattern {
    pub attributes: Vec<S<Opt<attribute::Attribute>>>,
    pub pattern: S<Pattern>,
}

#[derive_parsable]
impl SinglePattern {
    pub fn simple(pattern: S<Pattern>) -> SinglePattern {
        SinglePattern { attributes: vec![], pattern }
    }



    fn parser_inner<'a>(
        expr: ExprParser!('a),
        single_pattern: SPatParser!('a),
        default_allowed: bool
    ) -> impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone + 'a {
        attribute::outer_attribute().repeated().then(
            choice((
                // enum         -- path.path(data): type
                parse!(GenericArgPath)
                .then(DataPattern::parser(expr.clone(), single_pattern.clone()))
                .then(typ().or_not())
                        .map(|((path, pat), typ)|
                            Pattern::Enum { typ, pat, path }
                        ),
                // data         -- (data): type
                DataPattern::parser(expr.clone(), single_pattern)
                .then(typ().or_not())
                        .map(|(pat, typ)|
                            Pattern::Data { typ, pat }
                        ),
                // identifier   -- name: type @ bound
                    // name
                parse!(Ident)
                    .then_ignore(none_of([OP_DOT, OP_LPARA, OP_LCURLY, OP_LSQUARE]).rewind())
                .then(IdentifierInfo::parser(expr.clone(), OP_COLON, default_allowed))
                        .map(|(id, info)| Pattern::Identifier { id, info }),
                // rest pattern -- ...name
                rest()
                        .map(Pattern::rest_from_tuple),
                // bound        -- @ bound
                bound(expr) 
                        .map(Pattern::Bound),
            )).labelled("pattern variant").map_with_span(map_span)
        ).map_with_span(|(attributes, pattern), spn|
            Box::new(map_span(SinglePattern { attributes, pattern }, spn))
        ).labelled("pattern").boxed()
    }



    /// Parses a [`SinglePattern`] with a given `expression_parser` and no defaults allowed in the root (used for let statements)
    pub fn parser_no_default<'a>(expression_parser: ExprParser!('a)) -> impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone + 'a {
        Self::parser_inner(
            expression_parser.clone(), 
            Self::parser_with_expr(expression_parser), 
            false
        )
    }

    /// Parses a [`SinglePattern`] with a given `expression_parser` to allow for recursion
    pub fn parser_with_expr<'a>(expression_parser: ExprParser!('a)) -> impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone + 'a {
        recursive(|single_pattern|
            Self::parser_inner(
                expression_parser, 
                single_pattern, 
                true
            )
        )
    }

    /// Parses a [`SinglePattern`] 
    pub fn parser() -> impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone {
        Self::parser_with_expr(parse!(Expression))
    }
}

// conversion from pattern to single pattern
impl From<S<Pattern>> for S<SinglePattern> {
    fn from(pattern: S<Pattern>) -> Self {
        Spanned(pattern.span(), SinglePattern::simple(pattern))
    }
}

// conversion from data pattern to single pattern
impl From<S<DataPattern>> for S<SinglePattern> {
    fn from(pat: S<DataPattern>) -> Self {
        Spanned::new(pat.span(), SinglePattern::simple(
            Spanned::new(pat.span(), Pattern::Data { typ: None, pat })
        ))
    }
}

// conversion from pattern to boxed single pattern
impl From<S<Pattern>> for Box<S<SinglePattern>> {
    fn from(pattern: S<Pattern>) -> Self {
        Box::new(pattern.into())
    }
}

// conversion from data pattern to boxed single pattern
impl From<S<DataPattern>> for Box<S<SinglePattern>> {
    fn from(pattern: S<DataPattern>) -> Self {
        Box::new(pattern.into())
    }
}





#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pattern {
    Bound      ( S<Expression> ),
    Identifier { id: S<Ident>, info: S<IdentifierInfo> },

    Enum { path: S<GenericArgPath>, typ: Option<S<Type>>, pat: S<DataPattern>,  },
    Data                       { typ: Option<S<Type>>, pat: S<DataPattern>,  },
    Rest                       { typ: Option<S<Type>>, id: Option<S<Ident>>, },
}

impl Pattern {
    pub fn id(id: S<Ident>, info: S<IdentifierInfo>) -> Pattern { Self::Identifier { id, info } }

    fn rest_from_tuple(tuple: (Option<S<Ident>>, Option<S<Type>>)) -> Pattern {
        Pattern::Rest{ id: tuple.0, typ: tuple.1 }
    }
}




#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IdentifierInfo {
    pub typ:     Option<S<Type>>,
    pub bound:   Option<S<Expression>>,
    pub default: Option<S<Expression>>,
}

impl IdentifierInfo {
    pub fn new(typ: Option<S<Type>>, bound: Option<S<Expression>>, default: Option<S<Expression>>) -> IdentifierInfo {
        IdentifierInfo { typ, bound, default }
    }

    pub fn empty() -> IdentifierInfo {
        IdentifierInfo::new(None, None, None)
    }

    #[inline]
    fn from_tuple(((typ, bound), default): ((Option<S<Type>>, Option<S<Expression>>), Option<S<Expression>>)) -> IdentifierInfo {
        IdentifierInfo::new(typ, bound, default)
    }



    pub fn has_type    (&self) -> bool { self.  typ  .is_some() }
    pub fn has_bound   (&self) -> bool { self. bound .is_some() }
    pub fn has_default (&self) -> bool { self.default.is_some() }



    fn parser<'a>(
        expr: impl Parser<Token, S<Expression>, Error = Simple<Token>> + Clone + 'a, 
        type_token: Token, 
        default_allowed: bool
    ) -> impl Parser<Token, S<IdentifierInfo>, Error = Simple<Token>> + 'a {
        // : type
        typ_tok(type_token).or_not()
        // @ bound
        .then(bound(expr.clone()).or_not())
        // = default
        .then(if default_allowed { default(expr).or_not().boxed() } else { empty().to(None).boxed() })
                .map(IdentifierInfo::from_tuple)
                .map_with_span(map_span).labelled("identifier info")
    }
}





#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataPattern {
    List     ( Opt<Vec<Box<S<SinglePattern>>>>   ),
    Tuple    ( Opt<Vec<Box<S<SinglePattern>>>>   ),
    Compound ( CompoundPattern ),
}

impl DataPattern {
    /// Creates a [`DataPattern::List`] from a [`Vec`] of [`SinglePattern`]s
    pub fn list(list: Vec<S<SinglePattern>>) -> DataPattern {
        Self::List(Ok(list.into_iter().map(Box::new).collect()))
    }

    /// Creates a [`DataPattern::Tuple`] from a [`Vec`] of [`SinglePattern`]s
    pub fn tuple(list: Vec<S<SinglePattern>>) -> DataPattern {
        Self::Tuple(Ok(list.into_iter().map(Box::new).collect()))
    }

    /// Creates a [`DataPattern::Compound`] from a [`Vec`] of [`CompoundPatternField`]s
    pub fn compound(list: Vec<S<CompoundPatternField>>) -> DataPattern {
        Self::Compound(CompoundPattern(Ok(list)))
    }



    #[inline]
    fn pattern_list(single_pattern: SPatParser!()) -> impl Parser<Token, Vec<Box<S<SinglePattern>>>, Error = Simple<Token>> {
        // pattern,
        single_pattern
            .separated_by(just(OP_COMM)).allow_trailing()
    }

    pub fn parser<'a>(expr: ExprParser!('a), single_pattern: SPatParser!('a)) -> impl Parser<Token, S<DataPattern>, Error = Simple<Token>> + 'a {
        choice((
            // tuple (pattern,)
            Self::pattern_list(single_pattern.clone()).delimited_by(just(OP_LPARA), just(OP_RPARA))
                .map(Ok)
                .recover_with(nested_delimiters(OP_LPARA, OP_RPARA, [(OP_LSQUARE, OP_RSQUARE), (OP_LCURLY, OP_RCURLY)], |_| Err))
                .map(DataPattern::Tuple).labelled("tuple pattern"),
            // list [pattern,]
            Self::pattern_list(single_pattern.clone()).delimited_by(just(OP_LSQUARE), just(OP_RSQUARE))
                .map(Ok)
                .recover_with(nested_delimiters(OP_LSQUARE, OP_RSQUARE, [(OP_LPARA, OP_RPARA), (OP_LCURLY, OP_RCURLY)], |_| Err))
                .validate(Self::validate_list).labelled("list pattern"),
            CompoundPattern::parser_inner(expr, single_pattern)
                .map(DataPattern::Compound)
        )).map_with_span(map_span).labelled("data pattern")
    }


    fn validate_list(list: Opt<Vec<Box<Spanned<SinglePattern>>>>, span: Span, emit: &mut dyn FnMut(Simple<Token>)) -> DataPattern {
        if list.is_ok() && list.len() > 1 {
            // initialize type
            let mut typ = None;
    
            // for each pattern in list
            for pattern in list.clone().unwrap().into_iter() {
                let pattern = pattern.unspan().pattern.unspan(); // unwrap
    
                // if it's not a rest pattern
                if !matches!(pattern, Pattern::Rest{..}) {
                    // get the discriminant
                    let current = std::mem::discriminant(&pattern);
    
                    // if the type exists
                    if let Some(typ) = typ {
                        // check if it's the same
                        if current != typ { emit(Simple::custom(span.clone(), 
                            // TODO: change this to a custom reason
                            "Lists must be completely consisted of the same type."
                        )) }
                    // otherwise initialize it
                    } else { typ = Some(current); }
                }
            }
        }

        DataPattern::List(list)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CompoundPattern( Opt<Vec<S<CompoundPatternField>>> );

#[derive_parsable]
impl CompoundPattern {
    pub fn parser_inner<'a>(expr: ExprParser!('a), single_pattern: SPatParser!('a)) -> impl Parser<Token, CompoundPattern, Error = Simple<Token>> + Clone + 'a {
        recursive(|compound_pattern|
            // compound {field,}
            CompoundPatternField::parser(expr, single_pattern, compound_pattern)
                // field, field,
                .separated_by(just(OP_COMM)).allow_trailing()
                    // { field, }
                    .delimited_by(just(OP_LCURLY), just(OP_RCURLY))
                    .map(Ok).recover_with(nested_delimiters(OP_LCURLY, OP_RCURLY, [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE)], |_| Err))
                        .map(CompoundPattern).labelled("compound pattern")
        )
    }

    pub fn parser() -> impl Parser<Token, CompoundPattern, Error = Simple<Token>> + Clone {
        Self::parser_inner(parse!(Expression), parse!(SinglePattern))
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternField {
    Rest    { id: Option<S<Ident>>, typ: Option<S<Type>>, },
    Simple  { key: CompoundPatternKey, info: S<IdentifierInfo>},
    Pattern { key: CompoundPatternKey, pattern: Box<S<SinglePattern>>, },
}

impl CompoundPatternField {
    fn rest_from_tuple(tuple: (Option<S<Ident>>, Option<S<Type>>)) -> CompoundPatternField {
        CompoundPatternField::Rest { id: tuple.0, typ: tuple.1 }
    }


    /// Creates a rest field with an optional [`Ident`] and [`Type`]
    pub fn rest(id: Option<S<Ident>>, typ: Option<S<Type>>) -> CompoundPatternField {
        CompoundPatternField::Rest { id, typ }
    }

    /// Creates a simple field with a [key](CompoundPatternKey) and some [`IdentifierInfo`]
    pub fn simple(key: impl Into<CompoundPatternKey>, info: S<IdentifierInfo>) -> CompoundPatternField {
        CompoundPatternField::Simple { key: key.into(), info }
    }

    /// Creates a full field with a [key](CompoundPatternKey) and a [pattern](SinglePattern)
    pub fn pattern(key: impl Into<CompoundPatternKey>, pattern: impl Into<S<SinglePattern>>) -> CompoundPatternField {
        CompoundPatternField::Pattern { key: key.into(), pattern: Box::new(pattern.into()) }
    }



    fn parser<'a>(
        expr: ExprParser!('a), 
        single_pattern: SPatParser!('a), 
        compound_pattern: impl Parser<Token, pattern::CompoundPattern, Error = Simple<Token>> + Clone + 'a
    ) -> impl Parser<Token, S<CompoundPatternField>, Error = Simple<Token>> + 'a {
        // key as type @ bound = default | key: pattern | key | ...
        choice((
            // ...name
            rest()
                    .map(CompoundPatternField::rest_from_tuple),
            // key: pattern
            CompoundPatternKey::parser_inner(compound_pattern.clone())
            .then_ignore(just(OP_COLON))
            .then(single_pattern)
                    .map(|(key, pattern)| CompoundPatternField::Pattern { key, pattern }),
            // key as type @ bound = default
            CompoundPatternKey::parser_inner(compound_pattern)
            .then(IdentifierInfo::parser(expr, KW_AS, true))
                    .map(|(key, info)| CompoundPatternField::Simple { key, info }),
        )).map_with_span(map_span).labelled("compound pattern field")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternKey {
    Path(S<IndexedPath>),
    Index(S<usize>),
}

impl From<S<IndexedPath>> for CompoundPatternKey {
    fn from(v: S<IndexedPath>) -> Self {
        Self::Path(v)
    }
}

// // conversion from ident to ident key
// impl From<S<Ident>> for CompoundPatternKey {
//     fn from(id: S<Ident>) -> Self {
//         CompoundPatternKey::Ident(id)
//     }
// }

// conversion from index to index key
impl From<S<usize>> for CompoundPatternKey {
    fn from(ind: S<usize>) -> Self {
        CompoundPatternKey::Index(ind)
    }
}

impl CompoundPatternKey {
    pub fn parser_inner(compound_pattern: impl Parser<Token, pattern::CompoundPattern, Error = Simple<Token>> + Clone) -> impl Parser<Token, CompoundPatternKey, Error = Simple<Token>> {
        IndexedPath::parser_inner(compound_pattern).map(CompoundPatternKey::Path)
        .or(filter(Token::is_int)
            .map_with_span(|tok, spn|
                if let INTEGER(x) = tok { CompoundPatternKey::Index(map_span(x, spn)) }
                else { unreachable!() } // unreachable due to filter
            )
        ).labelled("compound pattern key")
    }

    pub fn parser() -> impl Parser<Token, CompoundPatternKey, Error = Simple<Token>> {
        Self::parser_inner(parse!(pattern::CompoundPattern))
    }
}




fn bound(expr: impl Parser<Token, S<Expression>, Error = Simple<Token>>) -> impl Parser<Token, S<Expression>, Error = Simple<Token>> {
    just(OP_AT)
        .ignore_then(expr)
        .labelled("pattern bound")
}

fn default(expr: impl Parser<Token, S<Expression>, Error = Simple<Token>>) -> impl Parser<Token, S<Expression>, Error = Simple<Token>> {
    just(OP_EQUAL)
        .ignore_then(expr)
        .labelled("pattern default")
}

fn rest() -> impl Parser<Token, (Option<S<Ident>>, Option<S<Type>>), Error = Simple<Token>> {
    just(OP_DOT).repeated().exactly(3) 
        .ignore_then(parse!(Ident).or_not())
        .then(typ().or_not())
        .labelled("rest pattern")
}

fn typ_tok(tok: Token) -> impl Parser<Token, S<Type>, Error = Simple<Token>> {
    just(tok)
        .ignore_then(parse!(Type))
        .labelled("pattern type")
}

fn typ() -> impl Parser<Token, S<Type>, Error = Simple<Token>> {
    typ_tok(OP_COLON)
}



#[cfg(test)]
mod tests {
    use super::{*, Pattern::*};
    use crate::tests::prelude::*;
    use span as s;

    #[test]
    fn real_world() {
        test_parser(indoc! {r#"
                // real world
                {
                    id,
                    Slot: @ 1,
                    tag: {
                        Enchantments: [
                            {
                                id: ench_id,
                                lvl as int
                            }: _,
                            ...
                        ]
                    }
                }
            "#},
            parse!(SinglePattern),
            // real world
            s(14..195, DataPattern::compound(vec![
                // id
                s(20..22, CompoundPatternField::simple(
                    IndexedPath::parse_offset(20, "id"),
                    s(22..22, IdentifierInfo::empty())
                )),
                // Slot: @ -1
                s(28..37, CompoundPatternField::pattern(
                    IndexedPath::parse_offset(28, "Slot"),
                    s(34..37, Bound(s(36..37, Expression::Temp)))
                )),
                // tag: {}
                s(43..193, CompoundPatternField::pattern(
                    IndexedPath::parse_offset(43, "tag"),
                    s(48..193, DataPattern::compound(vec![
                        // Enchantments: []
                        s(58..187, CompoundPatternField::pattern(
                            IndexedPath::parse_offset(58, "Enchantments"),
                            s(72..187, DataPattern::list(vec![
                                // {}: _
                                s(86..160, Pattern::Data{
                                    // {}
                                    pat: s(86..157, DataPattern::compound(vec![
                                        // id: ench_id
                                        s(104..115, CompoundPatternField::pattern(
                                            IndexedPath::parse_offset(104, "id"),
                                            // ench_id
                                            s(108..115, Pattern::id(
                                                s(108..115, Ident::from("ench_id")),
                                                s(115..115, IdentifierInfo::empty())
                                            ))
                                        )),
                                        // lvl as int
                                        span(133..143, CompoundPatternField::simple(
                                            IndexedPath::parse_offset(133, "lvl"),
                                            span(137..143, IdentifierInfo::new(Some(s(140..143, Type::Temp)), None, None))
                                        )),
                                    ])),
                                    // : _
                                    typ: Some(s(159..160, Type::Temp)),
                                }).into(),
                                // ...
                                s(174..177, Pattern::Rest { typ: None, id: None }).into()
                            ]))
                        ))
                    ]))
                ))
            ])).into(),
            HashMap::new()
        )
    }

    #[test]
    fn error_in_compound_check() {
        test_parser(indoc! {r#"
                // error in compound check
                {
                    test: {
                        test: {
                            *
                        }
                    }
                };
            "#},
            parse!(SinglePattern),
            // error in compound check
            s(27..88, DataPattern::compound(vec![
                s(33..86, CompoundPatternField::pattern(
                    IndexedPath::parse_offset(33, "test"),
                    s(39..86, DataPattern::compound(vec![
                        s(49..80, CompoundPatternField::pattern(
                            IndexedPath::parse_offset(49, "test"),
                            s(55..80, DataPattern::Compound(CompoundPattern(Err)))
                        )),
                    ]))
                )),
            ])).into(),
            HashMap::from([
                (69..70, (SimpleReason::Unexpected, Some(OP_STAR))),
            ])
        )
    }

    #[test]
    fn enum_ident_error_check() {
        test_parser(indoc! {r#"
                // enum-ident error check
                enumm.path(*);
            "#},
            parse!(SinglePattern),
            // enum-ident error check
            s(26..39, Pattern::Enum {
                path: s(26..36, GenericArgPath::new(Path::parse_offset(26, "enumm.path"), None)),
                pat: s(36..39, DataPattern::Tuple(Err)),
                typ: None,
            }).into(),
            HashMap::from([
                (37..38, (SimpleReason::Unexpected, Some(OP_STAR))),
            ])
        )
    }

    #[test]
    fn semi_full_check() {
        test_parser(indoc! {r#"
                // semi-full check
                this.name<generics>(
                    _name: type @ 1,
                    [
                        _ @ 1,
                        {
                            foo @ 1,
                            key as type,
                            ...rest: _,
                        }
                    ],
                )
            "#},
            parse!(SinglePattern),
            // semi-full check
            s(19..180, Pattern::Enum {
                // this.name<generics>()
                path: s(19..38, GenericArgPath::new(
                    Path::parse_offset(19, "this.name"),
                    Some(GenericArguments::parse_offset(28, "<generics>"))
                )),
                pat: s(38..180, DataPattern::tuple(vec![
                    // _name: type @ -3
                    s(44..59, Pattern::id(
                        s(44..49, Ident::from("_name")),
                        s(49..59, IdentifierInfo::new(
                            Some(Type::parse_offset(51, "type")),
                            Some(Expression::parse_offset(58, "1")),
                            None
                        ))
                    )).into(),
                    // []
                    s(65..177, DataPattern::list(vec![
                        // _ @ -3
                        s(75..80, Pattern::id(
                            s(75..76, Ident::from("_")),
                            s(77..80, IdentifierInfo::new(
                                None, Some(Expression::parse_offset(79, "1")), None
                            ))
                        )).into(),
                        // {}
                        s(90..171, DataPattern::compound(vec![
                            // foo @ -3
                            span(104..111, CompoundPatternField::simple(
                                IndexedPath::parse_offset(104, "foo"),
                                span(108..111, IdentifierInfo::new(None, Some(Expression::parse_offset(110, "1")), None))
                            )),
                            // key as type
                            span(125..136, CompoundPatternField::simple(
                                IndexedPath::parse_offset(125, "key"),
                                span(129..136, IdentifierInfo::new(Some(Type::parse_offset(132, "type")), None, None))
                            )),
                            // ...rest: _
                            span(150..160, CompoundPatternField::Rest {
                                id: Some(s(153..157, Ident::from("rest"))),
                                typ: Some(Type::parse_offset(159, "_"))
                            })
                        ])).into(),
                    ])).into()
                ])),
                typ: None,
            }).into(),
            HashMap::from([
                (65..177, (SimpleReason::Custom("Lists must be completely consisted of the same type.".to_string()), None))
            ])
        )
    }

    #[test]
    fn indexed_paths() {
        test_parser(indoc! {r#"
                {
                    item.tag.Inventory[{Slot @ 1}].tag."has space"{id @ 1}.list[0]: ident
                }
            "#},
            parse!(SinglePattern),
            s(0..77, DataPattern::compound(vec![
                s(6..75, CompoundPatternField::pattern(
                    IndexedPath::parse_offset(6, r#"item.tag.Inventory[{Slot @ 1}].tag."has space"{id @ 1}.list[0]"#),
                    s(70..75, Pattern::id(
                        s(70..75, "ident".into()),
                        s(76..75, IdentifierInfo::empty())
                    ))
                ))
            ])).into(),
            HashMap::new()
        )
    }
}

