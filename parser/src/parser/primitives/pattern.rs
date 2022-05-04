use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct MultiPattern {
    pub patterns: Vec<SinglePattern>,
}

impl MultiPattern {
    pub fn new(patterns: Vec<SinglePattern>) -> MultiPattern {
        MultiPattern { patterns }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SinglePattern {
    pub attributes: Vec<Opt<attribute::Attribute>>,
    pub pattern: Pattern,
}

impl SinglePattern {
    pub fn simple(pattern: Pattern) -> SinglePattern {
        SinglePattern { attributes: vec![], pattern }
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Pattern {
    Rest(Option<Ident>),
    Bound(Expression),

    Identifier(IdentifierPattern),

    Data(DataPattern)
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IdentifierPattern {
    pub wildcard: bool,
    pub ident: Option<Ident>,
    pub bound: Option<Expression>,
    pub typ: Option<Type>,
}

impl IdentifierPattern {
    fn new(wildcard: bool, ident: Option<Ident>, typ: Option<Type>, bound: Option<Expression>) -> IdentifierPattern {
        IdentifierPattern { wildcard, ident, typ, bound }
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataPattern {
    Enum { path: Path, pattern: Box<DataPattern> },

    List(Opt<Box<Vec<SinglePattern>>>),
    Tuple(Opt<Box<Vec<SinglePattern>>>),
    Compound(Opt<Vec<CompoundPatternField>>),
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternField {
    Rest(Option<Ident>),
    Simple(CompoundPatternKey),
    Typed { key: CompoundPatternKey, typ: Type, },
    Bounded { key: CompoundPatternKey, bound: Box<Expression>, },
    Pattern { key: CompoundPatternKey, pattern: Box<SinglePattern>, },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum CompoundPatternKey {
    Ident(Ident),
    Index(usize),
}



//TODO: add argument for expression parser needed due to recursion
fn bound() -> impl Parser<Token, Expression, Error = Simple<Token>> {
    just(OP_AT)
        .ignore_then(expression::expression())
}

fn rest() -> impl Parser<Token, Option<Ident>, Error = Simple<Token>> {
    just(OP_DOT).repeated().exactly(3) 
        .ignore_then(ident::ident().or_not())
}

fn typ() -> impl Parser<Token, Type, Error = Simple<Token>> {
    just(OP_COLON)
        .ignore_then(typ::typ())
}

fn key() -> impl Parser<Token, CompoundPatternKey, Error = Simple<Token>> {
    ident::ident().map(CompoundPatternKey::Ident)
    .or(filter(|tok| matches!(tok, INTEGER(_)))
        .map(|tok| 
            if let INTEGER(x) = tok { CompoundPatternKey::Index(x) }
            else { unreachable!() } // unreachable due to filter
        )
    )
}



fn data_pattern(single_pattern: impl Parser<Token, SinglePattern, Error = Simple<Token>> + Clone) -> impl Parser<Token, DataPattern, Error = Simple<Token>> {
    let pattern_list =
        single_pattern.clone()
            .separated_by(just(OP_COMM)).allow_trailing();
    
    choice((
        // tuple (pattern,)
        pattern_list.clone().delimited_by(just(OP_LPARA), just(OP_RPARA))
            .map(Box::new)
            .map(Ok).recover_with(nested_delimiters(OP_LPARA, OP_RPARA, [(OP_LSQUARE, OP_RSQUARE), (OP_LCURLY, OP_RCURLY)], Err))
            .map(DataPattern::Tuple),
        // list [pattern,]
        //TODO: add same type check
        pattern_list.delimited_by(just(OP_LSQUARE), just(OP_RSQUARE))
            .map(Box::new)
            .map(Ok).recover_with(nested_delimiters(OP_LSQUARE, OP_RSQUARE, [(OP_LPARA, OP_RPARA), (OP_LCURLY, OP_RCURLY)], Err))
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
                    .map(CompoundPatternField::Rest),
            // key
            key()
                // ensure that this doesn't take one of the above when it has an error, and then break everything afterwards
                .then_ignore(just(OP_COMM).rewind())
                    .map(CompoundPatternField::Simple)
        // field, field,
        )).separated_by(just(OP_COMM)).allow_trailing() 
            // { field, }
            .delimited_by(just(OP_LCURLY), just(OP_RCURLY)) 
            .map(Ok).recover_with(nested_delimiters(OP_LCURLY, OP_RCURLY, [(OP_LPARA, OP_RPARA), (OP_LSQUARE, OP_RSQUARE)], Err))
                .map(DataPattern::Compound),
    ))
}

pub fn pattern() -> impl Parser<Token, SinglePattern, Error = Simple<Token>> {
    recursive(|single_pattern| 
        attribute::outer_attribute().repeated()
        .then(
            choice((
                // data         -- (data), path.path[data]
                path::path().or_not()
                .then(data_pattern(single_pattern))
                        .map(|(path, pattern)| 
                            Pattern::Data(
                                // if it's an enum
                                if let Some(path) = path { 
                                    // package it
                                    DataPattern::Enum { pattern: Box::new(pattern), path, } 
                                // or just use it normally
                                } else { pattern }
                            )
                        ),
                // identifier   -- _name: type @ bound
                just(OP_UNDER)
                    .or_not()
                    .map(|x| x.is_some())
                    // name
                .then(ident::ident()
                    .then_ignore(none_of([OP_DOT, OP_LPARA, OP_LCURLY, OP_LSQUARE]).rewind()))
                    // : type
                .then(typ().or_not())
                    // @ bound
                .then(bound().or_not())
                        .map(|(((wildcard, ident), typ), bound)| 
                            Pattern::Identifier(IdentifierPattern::new(wildcard, Some(ident), typ, bound))
                        ),
                // rest pattern -- ...name
                rest()
                        .map(Pattern::Rest),
                // bound        -- @ bound
                bound() 
                        .map(Pattern::Bound),
                // wildcard     -- _: type @ bound
                just(OP_UNDER)
                .ignore_then(typ().or_not())
                .then(bound().or_not())
                        .map(|(typ, bound)| Pattern::Identifier(IdentifierPattern::new(true, None, typ, bound))),
            ))
        ).map(|(attributes, pattern)| SinglePattern { attributes, pattern })
    )
}

pub fn multi_pattern() -> impl Parser<Token, MultiPattern, Error = Simple<Token>> {
    pattern()
        .separated_by(just(OP_BAR))
        .map(MultiPattern::new)
}