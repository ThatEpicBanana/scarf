use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArgument {
    typ: Box<S<Type>>,
    id: Option<S<Ident>>,
}

#[derive_parsable]
impl GenericArgument {
    /// Creates a Generic Argument out of a spanned [`Type`] and optional [`Ident`] 
    pub fn new(typ: S<Type>, id: Option<S<Ident>>) -> GenericArgument {
        GenericArgument { typ: Box::new(typ), id }
    }

    /// Creates a simple Generic Argument out of just a [`Type`]
    pub fn simple(typ: S<Type>) -> GenericArgument {
        Self::new(typ, None)
    }

    //ADDDOC
    pub fn parser() -> impl Parser<Token, S<GenericArgument>, Error = Simple<Token>> {
        parse!(Ident)
            .then_ignore(just(OP_EQUAL))
            .or_not()
        .then(parse!(Type))
                .map(|(id, typ)| Self::new(typ, id))
                .map_with_span(map_span)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArguments(Vec<S<GenericArgument>>);

#[derive_parsable]
impl GenericArguments {
    /// Creates a new [`GenericArguments`] from a list of spanned [`GenericArgument`]s
    pub fn new(args: Vec<S<GenericArgument>>) -> GenericArguments {
        GenericArguments(args)
    }

    //ADDDOC
    pub fn parser() -> impl Parser<Token, S<Opt<GenericArguments>>, Error = Simple<Token>> {
        // arg
        parse!(GenericArgument)
        // arg, arg,
        .separated_by(just(OP_COMM))
            .allow_trailing()
        // <arg, arg,>
        .delimited_by(just(OP_LANGLE), just(OP_RANGLE))
                // map
                .map(GenericArguments::new)
                .map_with_span(map_ok_span)
                    .recover_with(nested_delimiters(OP_LANGLE, OP_RANGLE, [], err_span))
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericParameter {
    id: S<Ident>,
    types: Vec<S<Type>>
}

#[derive_parsable]
impl GenericParameter {
    pub fn new(id: S<Ident>, types: Vec<S<Type>>) -> GenericParameter {
        GenericParameter { id, types }
    }

    pub fn parser() -> impl Parser<Token, S<GenericParameter>, Error = Simple<Token>> {
        parse!(Ident)
        .then(
            just(OP_COLON)
                .ignore_then(parse!(Type))
                .separated_by(just(OP_PLUS))
            .or(empty().to(vec![]))
        ).map(|(id, typ)| Self::new(id, typ))
        .map_with_span(map_span)
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericParameters(Vec<S<GenericParameter>>);

#[derive_parsable]
impl GenericParameters {
    /// Creates a new [`GenericParameters`] from a list of spanned [`GenericParameter`]s
    pub fn new(args: Vec<S<GenericParameter>>) -> GenericParameters {
        GenericParameters(args)
    }

    //ADDDOC
    pub fn parser() -> impl Parser<Token, S<Opt<GenericParameters>>, Error = Simple<Token>> {
        // arg
        parse!(GenericParameter)
        // arg, arg,
        .separated_by(just(OP_COMM))
            .allow_trailing()
        // <arg, arg,>
        .delimited_by(just(OP_LANGLE), just(OP_RANGLE))
                // map
                .map(GenericParameters::new)
                .map_with_span(map_ok_span)
                    .recover_with(nested_delimiters(OP_LANGLE, OP_RANGLE, [], err_span))
    }
}
