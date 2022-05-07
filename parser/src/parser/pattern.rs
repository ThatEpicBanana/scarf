use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MultiPattern {
    pub patterns: Vec<S<SinglePattern>>,
}

impl MultiPattern {
    pub fn new(patterns: Vec<S<SinglePattern>>) -> MultiPattern {
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
    Bound      ( Box<S<Expression>> ),
    Identifier ( Box<S<IdentifierPattern>> ),

    Rest ( Option<S<Ident>>, Option<S<Type>> ),
    Data ( S<DataPattern> ),
}

impl Pattern {
    fn rest_from_tuple(tuple: (Option<S<Ident>>, Option<S<Type>>)) -> Pattern {
        Pattern::Rest(tuple.0, tuple.1)
    }
}

// wildcards are idents that start with an underscore
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IdentifierPattern {
    pub ident:   S<Ident>,
    pub typ:     Option<S<Type>>,
    pub bound:   Option<S<Expression>>,
    pub default: Option<S<Expression>>,
}

impl IdentifierPattern {
    fn new(ident: S<Ident>, typ: Option<S<Type>>, bound: Option<S<Expression>>, default: Option<S<Expression>>) -> IdentifierPattern {
        IdentifierPattern { ident, typ, bound, default }
    }

    pub fn has_type    (&self) -> bool { self.  typ  .is_some() }
    pub fn has_bound   (&self) -> bool { self. bound .is_some() }
    pub fn has_default (&self) -> bool { self.default.is_some() }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataPattern {
    Enum { path: S<Path>, pattern: Box<S<DataPattern>> },

    List     ( S<Opt<Box<Vec<S<SinglePattern>>>>>   ),
    Tuple    ( S<Opt<Box<Vec<S<SinglePattern>>>>>   ),
    Compound ( S<Opt<Vec<S<CompoundPatternField>>>> ),
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternField {
    Rest    ( Option<S<Ident>>, Option<S<Type>> ),
    Simple  ( CompoundPatternKey ),

    Typed   { key: CompoundPatternKey, typ: S<Type>, },
    Bounded { key: CompoundPatternKey, bound: Box<S<Expression>>, },
    Pattern { key: CompoundPatternKey, pattern: Box<S<SinglePattern>>, },
}

impl CompoundPatternField {
    fn rest_from_tuple(tuple: (Option<S<Ident>>, Option<S<Type>>)) -> CompoundPatternField {
        CompoundPatternField::Rest(tuple.0, tuple.1)
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



fn data_pattern(single_pattern: impl Parser<Token, S<SinglePattern>, Error = Simple<Token>> + Clone) -> impl Parser<Token, S<DataPattern>, Error = Simple<Token>> {
    let pattern_list =
        single_pattern.clone()
            .separated_by(just(OP_COMM)).allow_trailing();
    
    choice((
        // tuple (pattern,)
        pattern_list.clone().delimited_by(just(OP_LPARA), just(OP_RPARA))
            .map(Box::new)
            .map_with_span(ok_span)
            .recover_with(nested_delimiters(OP_LPARA, OP_RPARA, [(OP_LSQUARE, OP_RSQUARE), (OP_LCURLY, OP_RCURLY)], err_span))
            .map(DataPattern::Tuple),
        // list [pattern,]
        //TODO: add same type check
        pattern_list.delimited_by(just(OP_LSQUARE), just(OP_RSQUARE))
            .map(Box::new)
            .map_with_span(ok_span)
            .recover_with(nested_delimiters(OP_LSQUARE, OP_RSQUARE, [(OP_LPARA, OP_RPARA), (OP_LCURLY, OP_RCURLY)], err_span))
            .map(DataPattern::List),
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
                    .map(|(key, pattern)| CompoundPatternField::Pattern { key, pattern: Box::new(pattern) }),
            // key @ bound
            key()
            .then(bound())
                    .map(|(key, bound)| CompoundPatternField::Bounded { key, bound: Box::new(bound) }),
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
pub fn pattern() -> impl Parser<Token, S<SinglePattern>, Error = Simple<Token>> {
    recursive(|single_pattern| 
        attribute::outer_attribute().repeated()
        .then(
            choice((
                // data         -- (data), path.path[data]
                path::path().or_not()
                .then(data_pattern(single_pattern))
                        .map_with_span(|(path, pattern), spn| 
                            Pattern::Data(
                                // if it's an enum
                                if let Some(path) = path { 
                                    // package it
                                    span(DataPattern::Enum { pattern: Box::new(pattern), path }, spn)
                                // or just use it normally
                                } else { pattern }
                            )
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
                    .map(|(((ident, typ), default), bound)| IdentifierPattern::new(ident, typ, bound, default))
                    .map_with_span(span)
                    .map(Box::new)
                        // .validate(|pat, span, emit| {
                        //     //TODO: show both the bound and default in the error message?
                        //     // i think this is the realm of the error handler
                        //     // add some label with .with_label
                        //     // the error handler takes in the label and the ast, takes the bound and default spans from the ast, and displays them
                        //     // nevermind they can Slot: int @ 0..<36 = 0
                        //     // if pat.has_bound() && pat.has_default() { emit(Simple::custom(span, "Identifier patterns may not have both a bound and default")); }
                        //     Pattern::Identifier(pat)
                        // }),
                        .map(Pattern::Identifier),
                // rest pattern -- ...name
                rest()
                        .map(Pattern::rest_from_tuple),
                // bound        -- @ bound
                bound() 
                        .map(Box::new).map(Pattern::Bound),
            )).labelled("pattern variant").map_with_span(span)
        ).labelled("pattern").map_with_span(|(attributes, pattern), spn| span(SinglePattern { attributes, pattern }, spn))
    )
}

pub fn multi_pattern() -> impl Parser<Token, MultiPattern, Error = Simple<Token>> {
    pattern()
        .separated_by(just(OP_BAR))
        .map(MultiPattern::new)
}