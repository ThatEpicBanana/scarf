use crate::{parser::prelude::*, parse};

use super::prelude::primitives::path::IndexedPath;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MultiPattern {
    pub patterns: Vec<Box<S<SinglePattern>>>,
}

#[parser_util(derive_parsable,
    defaults(parse!(SinglePattern))
)]
impl MultiPattern {
    pub fn new(patterns: Vec<Box<S<SinglePattern>>>) -> MultiPattern {
        MultiPattern { patterns }
    }

    pub fn single(pattern: Box<S<SinglePattern>>) -> MultiPattern {
        Self::new(vec![pattern])
    }

    pub fn parser_inner(single_pattern: Box<S<SinglePattern>>) -> S<MultiPattern> {
        single_pattern
            .separated_by(just(OP_BAR))
            .map(MultiPattern::new)
                .map_with_span(map_span)
                .labelled("multi pattern")
    }
}




#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SinglePattern {
    pub attributes: Vec<S<Opt<Attribute>>>,
    pub pattern: S<Pattern>,
}

#[parser_util(derive_parsable)]
impl SinglePattern {
    pub fn simple(pattern: S<Pattern>) -> SinglePattern {
        SinglePattern { attributes: vec![], pattern }
    }



    fn parser_inner(
        expr: S<Expression>,
        single_pattern: Box<S<SinglePattern>>,
        #[no_convert] default_allowed: bool
    ) -> Box<S<SinglePattern>> {
        Attribute::outer_attribute().repeated().then(
            choice((
                // enum         -- path.path(data): type
                parse!(GenericArgPath)
                .then(parse!(DataPattern + expr.clone(), single_pattern.clone()))
                .then(typ().or_not())
                        .map(|((path, pat), typ)|
                            Pattern::Enum { typ, pat, path }
                        ),
                // data         -- (data): type
                parse!(DataPattern + expr.clone(), single_pattern.clone())
                .then(typ().or_not())
                        .map(|(pat, typ)|
                            Pattern::Data { typ, pat }
                        ),
                // identifier   -- name: type @ bound
                    // name
                parse!(Ident)
                    .then_ignore(none_of([OP_DOT, OP_LPARA, OP_LCURLY, OP_LSQUARE]).rewind())
                .then(parse!(IdentifierInfo + expr.clone(), single_pattern, OP_COLON, default_allowed))
                        .map(|(id, info)| Pattern::Identifier { id, info }),
                // rest pattern -- ...name
                rest()
                        .map(Pattern::rest_from_tuple),
                // bound        -- @ bound
                expression_bound(expr)
                        .map(Pattern::Bound),
            )).labelled("pattern variant").map_with_span(map_span)
        ).map_with_span(|(attributes, pattern), spn|
            Box::new(map_span(SinglePattern { attributes, pattern }, spn))
        ).labelled("pattern").boxed()
    }



    /// Parses a [`SinglePattern`] with a given `expression_parser` and no defaults allowed in the root (used for let statements)
    #[parser_fn(always_lifetime)]
    pub fn parser_no_default(expression_parser: S<Expression>) -> Box<S<SinglePattern>> {
        Self::parser_inner(
            expression_parser.clone(),
            Self::parser_with_expr(expression_parser),
            false
        )
    }

    /// Parses a [`SinglePattern`] with a given `expression_parser` to allow for recursion
    #[parser_fn(always_lifetime)]
    pub fn parser_with_expr(expression_parser: S<Expression>) -> Box<S<SinglePattern>> {
        recursive(|single_pattern|
            Self::parser_inner(
                expression_parser, 
                single_pattern,
                true
            )
        )
    }

    /// Parses a [`SinglePattern`]
    pub fn parser() -> Box<S<SinglePattern>> {
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
    pub bound:   Option<S<MultiPattern>>,
    pub default: Option<S<Expression>>,
}

#[parser_util]
impl IdentifierInfo {
    pub fn new(typ: Option<S<Type>>, bound: Option<S<MultiPattern>>, default: Option<S<Expression>>) -> IdentifierInfo {
        IdentifierInfo { typ, bound, default }
    }

    pub fn empty() -> IdentifierInfo {
        IdentifierInfo::new(None, None, None)
    }

    #[inline]
    fn from_tuple(((typ, bound), default): ((Option<S<Type>>, Option<S<MultiPattern>>), Option<S<Expression>>)) -> IdentifierInfo {
        IdentifierInfo::new(typ, bound, default)
    }



    pub fn has_type    (&self) -> bool { self.  typ  .is_some() }
    pub fn has_bound   (&self) -> bool { self. bound .is_some() }
    pub fn has_default (&self) -> bool { self.default.is_some() }



    pub fn parser_inner(
        expr: S<Expression>,
        s_pat: Box<S<SinglePattern>>,
        #[no_convert] type_token: Token,
        #[no_convert] default_allowed: bool
    ) -> S<IdentifierInfo> {
        // : type
        typ_tok(type_token).or_not()
        // @ bound
        .then(pattern_bound(s_pat).or_not())
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

#[parser_util(derive_parsable,
    defaults(parse!(Expression), parse!(SinglePattern))
)]
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



    #[inline] #[parser]
    fn pattern_list(single_pattern: Box<S<SinglePattern>>) -> Vec<Box<S<SinglePattern>>> {
        // pattern,
        single_pattern
            .separated_by(just(OP_COMM)).allow_trailing()
    }

    pub fn parser_inner(expr: S<Expression>, single_pattern: Box<S<SinglePattern>>) -> S<DataPattern> {
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
            parse!(CompoundPattern + expr, single_pattern)
                .map(DataPattern::Compound)
        )).map_with_span(map_span).labelled("data pattern")
    }


    fn validate_list(list: Opt<Vec<Box<Spanned<SinglePattern>>>>, span: Span, emit: &mut dyn FnMut(ParserError)) -> DataPattern {
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
                        if current != typ { emit(ParserError::from_reason(span, ParserErrorReason::PatternListSameType(list.clone().unwrap()))); break; }
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

#[parser_util(derive_parsable,
    defaults(parse!(Expression), parse!(SinglePattern))
)]
impl CompoundPattern {
    pub fn parser_inner(expr: S<Expression>, single_pattern: Box<S<SinglePattern>>) -> CompoundPattern {
        recursive(|compound_pattern|
            // compound {field,}
            parse!(CompoundPatternField + expr, single_pattern, compound_pattern)
                // field, field,
                .separated_by(just(OP_COMM)).allow_trailing()
                    // { field, }
                    .delimited_by(just(OP_LCURLY), just(OP_RCURLY))
                    .map(Ok).recover_with(nested_delimiters(OP_LCURLY, OP_RCURLY, [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE)], |_| Err))
                        .map(CompoundPattern).labelled("compound pattern")
        )
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternField {
    Rest    { id: Option<S<Ident>>, typ: Option<S<Type>>, },
    Simple  { key: CompoundPatternKey, info: S<IdentifierInfo>},
    Pattern { key: CompoundPatternKey, pattern: Box<S<SinglePattern>>, },
}

#[parser_util]
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



    fn parser_inner(
        expr: S<Expression>,
        single_pattern: Box<S<SinglePattern>>,
        compound_pattern: CompoundPattern
    ) -> S<CompoundPatternField> {
        // key as type @ bound = default | key: pattern | key | ...
        choice((
            // ...name
            rest()
                    .map(CompoundPatternField::rest_from_tuple),
            // key: pattern
            parse!(CompoundPatternKey + compound_pattern.clone())
            .then_ignore(just(OP_COLON))
            .then(single_pattern.clone())
                    .map(|(key, pattern)| CompoundPatternField::Pattern { key, pattern }),
            // key as type @ bound = default
            parse!(CompoundPatternKey + compound_pattern)
            .then(parse!(IdentifierInfo + expr, single_pattern, KW_AS, true))
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

// conversion from index to index key
impl From<S<usize>> for CompoundPatternKey {
    fn from(ind: S<usize>) -> Self {
        CompoundPatternKey::Index(ind)
    }
}

#[parser_util(derive_parsable,
    defaults(parse!(pattern::CompoundPattern)),
)]
impl CompoundPatternKey {
    pub fn parser_inner(compound_pattern: CompoundPattern) -> CompoundPatternKey {
        parse!(IndexedPath + compound_pattern).map(CompoundPatternKey::Path)
        .or(filter(Token::is_int)
            .map_with_span(|tok, spn|
                if let INTEGER(x) = tok { CompoundPatternKey::Index(map_span(x, spn)) }
                else { unreachable!() } // unreachable due to filter
            )
        ).labelled("compound pattern key")
    }
}


#[parser_fn]
fn pattern_bound(s_pat: Box<S<SinglePattern>>) -> S<MultiPattern> {
    just(OP_AT)
        .ignore_then(parse!(MultiPattern + s_pat))
        .labelled("pattern bound - pattern")
}

#[parser_fn]
fn expression_bound(expr: S<Expression>) -> S<Expression> {
    just(OP_AT)
        .ignore_then(expr)
        .labelled("pattern bound - expression")
}

#[parser_fn]
fn default(expr: S<Expression>) -> S<Expression> {
    just(OP_EQUAL)
        .ignore_then(expr)
        .labelled("pattern default")
}

#[parser_fn]
fn rest() -> (Option<S<Ident>>, Option<S<Type>>) {
    just(OP_DOT).repeated().exactly(3)
        .ignore_then(parse!(Ident).or_not())
        .then(typ().or_not())
        .labelled("rest pattern")
}

#[parser_fn]
fn typ_tok( #[no_convert] tok: Token ) -> S<Type> {
    just(tok)
        .ignore_then(parse!(Type))
        .labelled("pattern type")
}

#[parser_fn]
fn typ() -> S<Type> {
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
                    s(34..37, Bound(Expression::parse_offset(36, "1")))
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
                                            span(137..143, IdentifierInfo::new(Some(Type::parse_offset(140, "int")), None, None))
                                        )),
                                    ])),
                                    // : _
                                    typ: Some(Type::parse_offset(159, "_")),
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
                (69..70, (ParserErrorReason::Unexpected, Some(OP_STAR))),
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
                (37..38, (ParserErrorReason::Unexpected, Some(OP_STAR))),
            ])
        )
    }

    #[test]
    fn semi_full_check() {
        let array = vec![
                        // _ @ 1
                        s(76..82, Pattern::id(
                            s(76..77, Ident::from("_")),
                            s(78..82, IdentifierInfo::new(
                                None, Some(s(79..82, MultiPattern::single(s(79..82, Pattern::Bound(Expression::parse_offset(81, "1"))).into()))), None
                            ))
                        )).into(),
                        // {}
                        s(92..174, DataPattern::compound(vec![
                            // foo @ 1
                            span(106..114, CompoundPatternField::simple(
                                IndexedPath::parse_offset(106, "foo"),
                                span(110..114, IdentifierInfo::new(None, Some(s(111..114, MultiPattern::single(s(111..114, Pattern::Bound(Expression::parse_offset(113, "1"))).into()))), None))
                            )),
                            // key as typ
                            span(128..139, CompoundPatternField::simple(
                                IndexedPath::parse_offset(128, "key"),
                                span(132..139, IdentifierInfo::new(Some(Type::parse_offset(135, "Type")), None, None))
                            )),
                            // ...rest: _
                            span(153..163, CompoundPatternField::Rest {
                                id: Some(s(156..160, Ident::from("rest"))),
                                typ: Some(Type::parse_offset(162, "_"))
                            })
                        ])).into(),
                    ];

        test_parser(indoc! {r#"
                // semi-full check
                this.name<generics>(
                    _name: Type @@ 1,
                    [
                        _ @@ 1,
                        {
                            foo @@ 1,
                            key as Type,
                            ...rest: _,
                        }
                    ],
                )
            "#},
            parse!(SinglePattern),
            // semi-full check
            s(19..183, Pattern::Enum {
                // this.name<generics>()
                path: s(19..38, GenericArgPath::new(
                    Path::parse_offset(19, "this.name"),
                    Some(GenericArguments::parse_offset(28, "<generics>"))
                )),
                pat: s(38..183, DataPattern::tuple(vec![
                    // _name: typ @@ 1
                    s(44..60, Pattern::id(
                        s(44..49, Ident::from("_name")),
                        s(49..60, IdentifierInfo::new(
                            Some(Type::parse_offset(51, "Type")),
                            Some(s(57..60, MultiPattern::single(s(57..60, Pattern::Bound(Expression::parse_offset(59, "1"))).into()))),
                            None
                        ))
                    )).into(),
                    s(66..180, DataPattern::List(Ok(array.clone()))).into(),
                ])),
                typ: None,
            }).into(),
            HashMap::from([
                (66..180, (ParserErrorReason::PatternListSameType(array), None))
            ])
        )
    }

    #[test]
    fn indexed_paths() {
        test_parser(indoc! {r#"
                {
                    item.tag.Inventory[{Slot @@ 1}].tag."has space"{id @@ 1}.list[0]: ident
                }
            "#},
            parse!(SinglePattern),
            s(0..79, DataPattern::compound(vec![
                s(6..77, CompoundPatternField::pattern(
                    IndexedPath::parse_offset(6, r#"item.tag.Inventory[{Slot @@ 1}].tag."has space"{id @@ 1}.list[0]"#),
                    s(72..77, Pattern::id(
                        s(72..77, "ident".into()),
                        s(78..77, IdentifierInfo::empty())
                    ))
                ))
            ])).into(),
            HashMap::new()
        )
    }
}

