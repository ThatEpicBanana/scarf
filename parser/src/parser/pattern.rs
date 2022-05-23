use crate::{parser::prelude::*, parse};

// simple alias
macro_rules! SPatParser {
    () => { impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone };
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
            .map(MultiPattern::new)
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
        single_pattern: impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone + 'a,
        default_allowed: bool
    ) -> impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone + 'a {
        attribute::outer_attribute().repeated().then(
            choice((
                // enum         -- path.path(data): type
                parse!(GenericPath)
                .then(DataPattern::parser(single_pattern.clone()))
                .then(typ().or_not())
                        .map(|((path, pat), typ)|
                            Pattern::Enum { typ, pat, path }
                        ),
                // data         -- (data): type
                DataPattern::parser(single_pattern)
                .then(typ().or_not())
                        .map(|(pat, typ)| 
                            Pattern::Data { typ, pat }
                        ),
                // identifier   -- name: type @ bound
                    // name
                parse!(Ident)
                    .then_ignore(none_of([OP_DOT, OP_LPARA, OP_LCURLY, OP_LSQUARE]).rewind())
                .then(IdentifierInfo::parser(OP_COLON, default_allowed))
                        .map(|(id, info)| Pattern::Identifier { id, info }),
                // rest pattern -- ...name
                rest()
                        .map(Pattern::rest_from_tuple),
                // bound        -- @ bound
                bound() 
                        .map(Pattern::Bound),
            )).labelled("pattern variant").map_with_span(map_span)
        ).labelled("pattern").map_with_span(|(attributes, pattern), spn| 
            Box::new(map_span(SinglePattern { attributes, pattern }, spn))
        ).boxed()
    }

    /// Parses a [`SinglePattern`] with no defaults allowed in the root, used for let statements
    pub fn parser_no_default() -> SPatParser!() {
        Self::parser_inner(Self::parser(), false)
    }

    pub fn parser() -> impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone {
        recursive(|single_pattern| 
            Self::parser_inner(single_pattern, true)
        )
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

    fn parser(type_token: Token, default_allowed: bool) -> impl Parser<Token, S<IdentifierInfo>, Error = Simple<Token>> {
        // : type
        typ_tok(type_token).or_not()
        // @ bound
        .then(bound().or_not())
        // = default
        .then(if default_allowed { default().or_not().boxed() } else { empty().to(None).boxed() })
                .map(IdentifierInfo::from_tuple)
                .map_with_span(map_span)
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

    pub fn parser(single_pattern: SPatParser!()) -> impl Parser<Token, S<DataPattern>, Error = Simple<Token>> {
        choice((
            // tuple (pattern,)
            Self::pattern_list(single_pattern.clone()).delimited_by(just(OP_LPARA), just(OP_RPARA))
                .map(Ok)
                .recover_with(nested_delimiters(OP_LPARA, OP_RPARA, [(OP_LSQUARE, OP_RSQUARE), (OP_LCURLY, OP_RCURLY)], |_| Err))
                .map(DataPattern::Tuple),
            // list [pattern,]
            Self::pattern_list(single_pattern.clone()).delimited_by(just(OP_LSQUARE), just(OP_RSQUARE))
                .map(Ok)
                .recover_with(nested_delimiters(OP_LSQUARE, OP_RSQUARE, [(OP_LPARA, OP_RPARA), (OP_LCURLY, OP_RCURLY)], |_| Err))
                .validate(Self::validate_list),
            // compound {field,}
            CompoundPatternField::parser(single_pattern)
                // field, field,
                .separated_by(just(OP_COMM)).allow_trailing() 
                    // { field, }
                    .delimited_by(just(OP_LCURLY), just(OP_RCURLY)) 
                    .map(Ok).recover_with(nested_delimiters(OP_LCURLY, OP_RCURLY, [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE)], |_| Err))
                        .map(DataPattern::Compound),
        )).map_with_span(map_span)
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


    fn parser(single_pattern: SPatParser!()) -> impl Parser<Token, S<CompoundPatternField>, Error = Simple<Token>> {
        // key as type @ bound = default | key: pattern | key | ...
        choice((
            // key: pattern
            parse!(CompoundPatternKey)
            .then_ignore(just(OP_COLON))
            .then(single_pattern)
                    .map(|(key, pattern)| CompoundPatternField::Pattern { key, pattern }),
            // key as type @ bound = default
            parse!(CompoundPatternKey)
            .then(IdentifierInfo::parser(KW_AS, true))
                    .map(|(key, info)| CompoundPatternField::Simple { key, info }),
            // ...name
            rest()
                    .map(CompoundPatternField::rest_from_tuple),
        )).map_with_span(map_span)
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
        .or(filter(|tok| matches!(tok, INTEGER(_)))
            .map_with_span(|tok, spn| 
                if let INTEGER(x) = tok { CompoundPatternKey::Index(map_span(x, spn)) }
                else { unreachable!() } // unreachable due to filter
            )
        ).labelled("compound pattern key")
    }
}



//TODO: add argument for expression parser needed due to recursion
// replace parser() with inner_parser(expression) then add a parser() with the normal expression parser for parse!()
fn bound() -> impl Parser<Token, S<Expression>, Error = Simple<Token>> {
    just(OP_AT)
        .ignore_then(parse!(Expression))
        .labelled("pattern bound")
}

fn default() -> impl Parser<Token, S<Expression>, Error = Simple<Token>> {
    just(OP_EQUAL)
        .ignore_then(parse!(Expression))
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