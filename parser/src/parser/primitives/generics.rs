use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArgument {
    typ: Box<S<Type>>,
    id: Option<S<Ident>>,
}

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

impl GenericArguments {
    /// Creates a new [`GenericArguments`] from a list of spanned [`GenericArgument`]s
    pub fn new(args: Vec<S<GenericArgument>>) -> GenericArguments {
        GenericArguments(args)
    }

    
    pub fn from_offset_string(offset: usize, string: &str) -> S<Opt<GenericArguments>> {
        offset_string(offset, string).as_str().into()
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

impl From<&str> for S<Opt<GenericArguments>> {
    /// Converts a string into a [`GenericArguments`]
    /// 
    /// ### Panics
    /// 
    /// - If the lexer or parser fails
    fn from(string: &str) -> S<Opt<GenericArguments>> {
        lex_to_parse(string, parse!(GenericArguments), "GenericArguments")
    }
}