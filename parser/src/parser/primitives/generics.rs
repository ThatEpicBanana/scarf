use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArgument {
    typ: Box<S<Type>>,
    id: Option<S<Ident>>,
}

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
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
    pub fn parser_inner(typ: S<Type>) -> S<GenericArgument> {
        parse!(Ident)
            .then_ignore(just(OP_EQUAL))
            .or_not()
        .then(typ)
                .map(|(id, typ)| Self::new(typ, id))
                .map_with_span(map_span)
    }
}

impl From<S<Type>> for S<GenericArgument> {
    fn from(typ: S<Type>) -> Self {
        let spn = typ.span();
        Spanned::new(spn, GenericArgument::simple(typ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArguments(pub Vec<S<GenericArgument>>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl GenericArguments {
    /// Creates a new [`GenericArguments`] from a list of spanned [`GenericArgument`]s
    pub fn new(args: Vec<S<GenericArgument>>) -> GenericArguments {
        GenericArguments(args)
    }

    //ADDDOC
    pub fn parser_inner(typ: S<Type>) -> S<Opt<GenericArguments>> {
        // arg
        parse!(GenericArgument + typ)
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

#[parser_util(derive_parsable)]
impl GenericParameter {
    pub fn new(id: S<Ident>, types: Vec<S<Type>>) -> GenericParameter {
        GenericParameter { id, types }
    }

    pub fn parser() -> S<GenericParameter> {
        parse!(Ident)
        .then(
            just(OP_COLON).ignore_then(
                parse!(Type)
                    .separated_by(just(OP_PLUS))
            ).or(empty().to(vec![]))
        ).map(|(id, typ)| Self::new(id, typ))
        .map_with_span(map_span)
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericParameters(Vec<S<GenericParameter>>);

#[parser_util(derive_parsable)]
impl GenericParameters {
    /// Creates a new [`GenericParameters`] from a list of spanned [`GenericParameter`]s
    pub fn new(args: Vec<S<GenericParameter>>) -> GenericParameters {
        GenericParameters(args)
    }

    //ADDDOC
    pub fn parser() -> S<Opt<GenericParameters>> {
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


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::prelude::*;

    #[test]
    fn generic_arguments() {
        test_parser(indoc! {r#"
                <Type, x = Type>
            "#}, 
            parse!(GenericArguments),
            ok_span(0..16, GenericArguments::new(vec![
                span(1..5, GenericArgument::new(
                    Type::parse_offset(1, "Type"),
                    None
                )),
                span(7..15, GenericArgument::new(
                    Type::parse_offset(11, "Type"),
                    Some(span(7..8, "x".into()))
                ))
            ])), 
            HashMap::new()
        )
    }

    #[test]
    fn generic_parameters() {
        test_parser(indoc! {r#"
                <T, T: Raycaster + Stuff>
            "#},
            parse!(GenericParameters),
            ok_span(0..25, GenericParameters::new(vec![
                span(1..2, GenericParameter::new(
                    span(1..2, "T".into()),
                    vec![]
                )),
                span(4..24, GenericParameter::new(
                    span(4..5, "T".into()),
                    vec![
                        Type::parse_offset(7, "Raycaster"),
                        Type::parse_offset(19, "Stuff")
                    ]
                )),
            ])),
            HashMap::new()
        )
    }
}
