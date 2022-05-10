use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MultiPattern {
    pub patterns: Vec<Box<S<SinglePattern>>>,
}

impl MultiPattern {
    pub fn new(patterns: Vec<Box<S<SinglePattern>>>) -> MultiPattern {
        MultiPattern { patterns }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SinglePattern {
    pub attributes: Vec<S<Opt<attribute::Attribute>>>,
    pub pattern: S<Pattern>,
}

impl SinglePattern {
    pub fn simple(pattern: S<Pattern>) -> SinglePattern {
        SinglePattern { attributes: vec![], pattern }
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pattern {
    Bound      ( S<Expression> ),
    Identifier ( S<IdentifierPattern> ),

    Rest { typ: Option<S<Type>>, id: Option<S<Ident>>, },
    Data { typ: Option<S<Type>>, pat: S<DataPattern>,  },
}

impl Pattern {
    fn rest_from_tuple(tuple: (Option<S<Ident>>, Option<S<Type>>)) -> Pattern {
        Pattern::Rest{ id: tuple.0, typ: tuple.1 }
    }
}

// wildcards are idents that start with an underscore
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IdentifierPattern {
    pub id:   S<Ident>,
    pub typ:     Option<S<Type>>,
    pub bound:   Option<S<Expression>>,
    pub default: Option<S<Expression>>,
}

impl IdentifierPattern {
    fn new(id: S<Ident>, typ: Option<S<Type>>, bound: Option<S<Expression>>, default: Option<S<Expression>>) -> IdentifierPattern {
        IdentifierPattern { id, typ, bound, default }
    }

    pub fn has_type    (&self) -> bool { self.  typ  .is_some() }
    pub fn has_bound   (&self) -> bool { self. bound .is_some() }
    pub fn has_default (&self) -> bool { self.default.is_some() }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataPattern {
    Enum { path: S<Path>, pattern: Box<S<DataPattern>> },

    List     ( S<Opt<Vec<Box<S<SinglePattern>>>>>   ),
    Tuple    ( S<Opt<Vec<Box<S<SinglePattern>>>>>   ),
    Compound ( S<Opt<Vec<S<CompoundPatternField>>>> ),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternField {
    Rest    { id: Option<S<Ident>>, typ: Option<S<Type>>, },
    Simple  ( CompoundPatternKey ),

    Typed   { key: CompoundPatternKey, typ: S<Type>, },
    Bounded { key: CompoundPatternKey, bound: S<Expression>, },
    Pattern { key: CompoundPatternKey, pattern: Box<S<SinglePattern>>, },
}

impl CompoundPatternField {
    fn rest_from_tuple(tuple: (Option<S<Ident>>, Option<S<Type>>)) -> CompoundPatternField {
        CompoundPatternField::Rest { id: tuple.0, typ: tuple.1 }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternKey {
    Ident(S<Ident>),
    Index(S<usize>),
}



//TODO: add argument for expression parser needed due to recursion
fn bound() -> impl Parser<Token, S<Expression>, Error = Simple<Token>> {
    just(OP_AT)
        .ignore_then(expression::expression())
        .labelled("pattern bound")
}

fn default() -> impl Parser<Token, S<Expression>, Error = Simple<Token>> {
    just(OP_EQUAL)
        .ignore_then(expression::expression())
        .labelled("pattern default")
}

fn rest() -> impl Parser<Token, (Option<S<Ident>>, Option<S<Type>>), Error = Simple<Token>> {
    just(OP_DOT).repeated().exactly(3) 
        .ignore_then(ident::ident().or_not())
        .then(typ().or_not())
        .labelled("rest pattern")
}

fn typ() -> impl Parser<Token, S<Type>, Error = Simple<Token>> {
    just(OP_COLON)
        .ignore_then(typ::typ())
        .labelled("pattern type")
}

fn key() -> impl Parser<Token, CompoundPatternKey, Error = Simple<Token>> {
    ident::ident().map(CompoundPatternKey::Ident)
    .or(filter(|tok| matches!(tok, INTEGER(_)))
        .map_with_span(|tok, spn| 
            if let INTEGER(x) = tok { CompoundPatternKey::Index(span(x, spn)) }
            else { unreachable!() } // unreachable due to filter
        )
    ).labelled("compound pattern key")
}



fn data_pattern(single_pattern: impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> + Clone) -> impl Parser<Token, S<DataPattern>, Error = Simple<Token>> {
    let pattern_list =
        single_pattern.clone()
            .separated_by(just(OP_COMM)).allow_trailing();
    
    choice((
        // tuple (pattern,)
        pattern_list.clone().delimited_by(just(OP_LPARA), just(OP_RPARA))
            .map_with_span(ok_span)
            .recover_with(nested_delimiters(OP_LPARA, OP_RPARA, [(OP_LSQUARE, OP_RSQUARE), (OP_LCURLY, OP_RCURLY)], err_span))
            .map(DataPattern::Tuple),
        // list [pattern,]
        pattern_list.delimited_by(just(OP_LSQUARE), just(OP_RSQUARE))
            .map_with_span(ok_span)
            .recover_with(nested_delimiters(OP_LSQUARE, OP_RSQUARE, [(OP_LPARA, OP_RPARA), (OP_LCURLY, OP_RCURLY)], err_span))
            .validate(|list, span, emit| {
                // validate that all the patterns in the list are the same type
                if list.is_ok() && list.len() > 1 {
                    // initialize type
                    let mut typ = None;

                    // for each pattern in list
                    for pattern in list.clone().unwrap_span().into_iter() {
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
            }),
        // compound {key as type, key: pattern, key @ bound, key, ...}
        choice((
            // key as type
            key()
            .then_ignore(just(KW_AS))
            .then(typ::typ())
                    .map(|(key, typ)| CompoundPatternField::Typed { key, typ }),
            // key: pattern
            key()
            .then_ignore(just(OP_COLON))
            .then(single_pattern)
                    .map(|(key, pattern)| CompoundPatternField::Pattern { key, pattern }),
            // key @ bound
            key()
            .then(bound())
                    .map(|(key, bound)| CompoundPatternField::Bounded { key, bound }),
            // ...name
            rest()
                    .map(CompoundPatternField::rest_from_tuple),
            // key
            key()
                // ensure that this doesn't take one of the above when it has an error, and then break everything afterwards
                .then_ignore(just(OP_COMM).rewind())
                    .map(CompoundPatternField::Simple)
        // field, field,
        )).map_with_span(span)
        .separated_by(just(OP_COMM)).allow_trailing() 
            // { field, }
            .delimited_by(just(OP_LCURLY), just(OP_RCURLY)) 
            .map_with_span(ok_span).recover_with(nested_delimiters(OP_LCURLY, OP_RCURLY, [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE)], err_span))
                .map(DataPattern::Compound),
    )).map_with_span(span)
}

//FIXME: make a parser where the top level does not allow for defaults
pub fn pattern() -> impl Parser<Token, Box<S<SinglePattern>>, Error = Simple<Token>> {
    recursive(|single_pattern| 
        attribute::outer_attribute().repeated()
        .then(
            choice((
                // data         -- (data), path.path[data]
                //TODO: enum generics
                path::path().or_not()
                .then(data_pattern(single_pattern))
                .then(typ().or_not())
                        .map_with_span(|((path, pattern), typ), spn| 
                            Pattern::Data{
                                // optional type
                                typ,
                                // then if it's an enum
                                pat: if let Some(path) = path { 
                                    // package it
                                    span(DataPattern::Enum { pattern: Box::new(pattern), path }, spn)
                                // or just use it normally
                                } else { pattern },
                            }
                        ),
                // identifier   -- name: type @ bound
                    // name
                ident::ident()
                    .then_ignore(none_of([OP_DOT, OP_LPARA, OP_LCURLY, OP_LSQUARE]).rewind())
                    // : type
                .then(typ().or_not())
                    // = default
                .then(default().or_not())
                    // @ bound
                .then(bound().or_not())
                    .map_with_span(|(((ident, typ), default), bound), spn| 
                        Pattern::Identifier(span(
                            IdentifierPattern::new(ident, typ, bound, default), 
                            spn
                        ))
                    ),
                // rest pattern -- ...name
                rest()
                        .map(Pattern::rest_from_tuple),
                // bound        -- @ bound
                bound() 
                        .map(Pattern::Bound),
            )).labelled("pattern variant").map_with_span(span)
        ).labelled("pattern").map_with_span(|(attributes, pattern), spn| Box::new(span(SinglePattern { attributes, pattern }, spn)))
    )
}

pub fn multi_pattern() -> impl Parser<Token, MultiPattern, Error = Simple<Token>> {
    pattern()
        .separated_by(just(OP_BAR))
        .map(MultiPattern::new)
}