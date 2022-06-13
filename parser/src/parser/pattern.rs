use crate::{parser::prelude::*, parse};

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
                parse!(GenericPath)
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

    Enum { path: S<GenericPath>, typ: Option<S<Type>>, pat: S<DataPattern>,  },
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
    Compound ( Opt<Vec<S<CompoundPatternField>>> ),
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
        Self::Compound(Ok(list))
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
            // compound {field,}
            CompoundPatternField::parser(expr, single_pattern)
                // field, field,
                .separated_by(just(OP_COMM)).allow_trailing() 
                    // { field, }
                    .delimited_by(just(OP_LCURLY), just(OP_RCURLY))
                    .map(Ok).recover_with(nested_delimiters(OP_LCURLY, OP_RCURLY, [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE)], |_| Err))
                        .map(DataPattern::Compound).labelled("compound pattern"),
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
                            //TODO: change this to a custom reason
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



    fn parser<'a>(expr: ExprParser!('a), single_pattern: SPatParser!('a)) -> impl Parser<Token, S<CompoundPatternField>, Error = Simple<Token>> + 'a {
        // key as type @ bound = default | key: pattern | key | ...
        choice((
            // key: pattern
            parse!(CompoundPatternKey)
            .then_ignore(just(OP_COLON))
            .then(single_pattern)
                    .map(|(key, pattern)| CompoundPatternField::Pattern { key, pattern }),
            // key as type @ bound = default
            parse!(CompoundPatternKey)
            .then(IdentifierInfo::parser(expr, KW_AS, true))
                    .map(|(key, info)| CompoundPatternField::Simple { key, info }),
            // ...name
            rest()
                    .map(CompoundPatternField::rest_from_tuple),
        )).map_with_span(map_span).labelled("compound pattern field")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternKey {
    Ident(S<Ident>),
    Index(S<usize>),
}

// conversion from ident to ident key
impl From<S<Ident>> for CompoundPatternKey {
    fn from(id: S<Ident>) -> Self {
        CompoundPatternKey::Ident(id)
    }
}

// conversion from index to index key
impl From<S<usize>> for CompoundPatternKey {
    fn from(ind: S<usize>) -> Self {
        CompoundPatternKey::Index(ind)
    }
}

impl CompoundPatternKey {
    fn parser() -> impl Parser<Token, CompoundPatternKey, Error = Simple<Token>> {
        parse!(Ident).map(CompoundPatternKey::Ident)
        .or(filter(Token::is_int)
            .map_with_span(|tok, spn| 
                if let INTEGER(x) = tok { CompoundPatternKey::Index(map_span(x, spn)) }
                else { unreachable!() } // unreachable due to filter
            )
        ).labelled("compound pattern key")
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



#[test]
#[cfg(test)]
fn patterns() {
    use crate::tests::prelude::*;
    use chumsky::error::SimpleReason;
    use pattern::{*, Pattern::*};
    use span as s;

    // TODO: Seperate this into seperate functions
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
        };

        // error in compound check
        {
            test: {
                test: {
                    *
                }
            }
        };

        // enum-ident error check
        enumm.path(*);

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
        );
        "#},
        parse!(SinglePattern)
            // // useful for debugging, but breaks parser and adds an error at the end
            // .map_with_span(ok_span)
            // .recover_with(skip_until([OP_SEMI], err_span))
            .separated_by(just(OP_SEMI)).allow_trailing()
            .then_ignore(end()),
        vec![
            // real world
            s(14..195, DataPattern::compound(vec![
                // id
                s(20..22, CompoundPatternField::simple(
                    s(20..22, Ident::from("id")),
                    s(22..22, IdentifierInfo::empty())
                )),
                // Slot: @ -1
                s(28..37, CompoundPatternField::pattern(
                    s(28..32, Ident::from("Slot")),
                    s(34..37, Bound(s(36..37, Expression::Temp)))
                )),
                // tag: {}
                s(43..193, CompoundPatternField::pattern(
                    s(43..46, Ident::from("tag")),
                    s(48..193, DataPattern::compound(vec![
                        // Enchantments: []
                        s(58..187, CompoundPatternField::pattern(
                            s(58..70, Ident::from("Enchantments")),
                            s(72..187, DataPattern::list(vec![
                                // {}: _
                                s(86..160, Pattern::Data{
                                    // {}
                                    pat: s(86..157, DataPattern::compound(vec![
                                        // id: ench_id
                                        s(104..115, CompoundPatternField::pattern(
                                            s(104..106, Ident::from("id")),
                                            // ench_id
                                            s(108..115, Pattern::id(
                                                s(108..115, Ident::from("ench_id")),
                                                s(115..115, IdentifierInfo::empty())
                                            ))
                                        )),
                                        // lvl as int
                                        span(133..143, CompoundPatternField::simple(
                                            span(133..136, Ident::from("lvl")),
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
            // error in compound check
            s(225..286, DataPattern::compound(vec![
                s(231..284, CompoundPatternField::pattern(
                    s(231..235, Ident::from("test")),
                    s(237..284, DataPattern::compound(vec![
                        s(247..278, CompoundPatternField::pattern(
                            s(247..251, Ident::from("test")),
                            s(253..278, DataPattern::Compound(Err))
                        )),
                    ]))
                )),
            ])).into(),
            // enum-ident error check
            s(315..328, Pattern::Enum {
                path: s(315..325, GenericPath::new(Path::parse_offset(315, "enumm.path"), None)),
                pat: s(325..328, DataPattern::Tuple(Err)),
                typ: None,
            }).into(),
            // semi-full check
            s(350..511, Pattern::Enum {
                // this.name<generics>()
                path: s(350..369, GenericPath::new(
                    Path::parse_offset(350, "this.name"),
                    Some(GenericArguments::parse_offset(359, "<generics>"))
                )),
                pat: s(369..511, DataPattern::tuple(vec![
                    // _name: type @ -3
                    s(375..390, Pattern::id(
                        s(375..380, Ident::from("_name")),
                        s(380..390, IdentifierInfo::new(
                            Some(s(382..386, Type::Temp)),
                            Some(s(389..390, Expression::Temp)),
                            None
                        ))
                    )).into(),
                    // []
                    s(396..508, DataPattern::list(vec![
                        // _ @ -3
                        s(406..411, Pattern::id(
                            s(406..407, Ident::from("_")),
                            s(408..411, IdentifierInfo::new(
                                None, Some(s(410..411, Expression::Temp)), None
                            ))
                        )).into(),
                        // {}
                        s(421..502, DataPattern::compound(vec![
                            // foo @ -3
                            span(435..442, CompoundPatternField::simple(
                                span(435..438, Ident::from("foo")),
                                span(439..442, IdentifierInfo::new(None, Some(s(441..442, Expression::Temp)), None))
                            )),
                            // key as type
                            span(456..467, CompoundPatternField::simple(
                                span(456..459, Ident::from("key")),
                                span(460..467, IdentifierInfo::new(Some(s(463..467, Type::Temp)), None, None))
                            )),
                            // ...rest: _
                            span(481..491, CompoundPatternField::Rest {
                                id: Some(s(484..488, Ident::from("rest"))),
                                typ: Some(s(490..491, Type::Temp))
                            })
                        ])).into(),
                    ])).into()
                ])),
                typ: None,
            }).into(),
        ], 
        HashMap::from([
            (267..268, (SimpleReason::Unexpected, Some(OP_STAR))),
            (326..327, (SimpleReason::Unexpected, Some(OP_STAR))),
            (396..508, (SimpleReason::Custom("Lists must be completely consisted of the same type.".to_string()), None))
        ])
    );
}
