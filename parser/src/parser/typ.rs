use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
        Path(S<GenericArgPath>),
        Data(  DataType ),
    Function(S<FunctionType>)
}

#[parser_util(derive_parsable)]
impl Type {
    pub fn parser() -> S<Type> {
        recursive(|typ| parse!(Type + typ))
    }

    pub fn parser_inner(typ: S<Type>) -> S<Type> {
        choice((
            parse!(GenericArgPath + typ.clone()).map(Type::Path),
                  parse!(DataType + typ.clone()).map(Type::Data),
              parse!(FunctionType + typ        ).map(Type::Function),
        ))
            .labelled("type")
            .map_with_span(map_span)
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataType {
        Tuple(S<Opt<   TupleType>>),
         List(S<Opt<    ListType>>),
     Compound(S<Opt<CompoundType>>),
}

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl DataType {
    pub fn parser_inner(typ: S<Type>) -> DataType {
        choice((
             parse!(TupleType + typ.clone()).map(DataType::Tuple),
              parse!(ListType + typ        ).map(DataType::List),
        ))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TupleType(Vec<Box<S<Type>>>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl TupleType {
    pub fn parser_inner(typ: S<Type>) -> S<Opt<TupleType>> {
        typ.map(Box::new)
            // typ, typ
            .separated_by(just(op!(",")))
                // (typ, typ)
                .delimited_by(just(op!("(")), just(op!(")")))
                .map(TupleType).labelled("tuple type")
                    // error handling
                    .map_with_span(map_ok_span)
                    .recover_with(nested_delimiters(
                             op!("("), op!(")"),
                        [
                            (op!("["), op!("]")),
                            (op!("<"), op!(">")),
                            (op!("{"), op!("}")),
                        ],
                    err_span))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ListType(Box<S<Type>>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl ListType {
    pub fn parser_inner(typ: S<Type>) -> S<Opt<ListType>> {
        typ.map(Box::new)
            // [typ]
            .delimited_by(just(op!("[")), just(op!("]")))
            .map(ListType).labelled("list type")
                // error handling
                .map_with_span(map_ok_span)
                .recover_with(nested_delimiters(
                         op!("["), op!("]"),
                    [
                        (op!("("), op!(")")),
                        (op!("<"), op!(">")),
                        (op!("{"), op!("}")),
                    ],
                err_span))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CompoundTypeItem {
    ident:     S<Ident>,
      typ: Box<S<Type>>,
}

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl CompoundTypeItem {
    pub fn parser_inner(typ: S<Type>) -> S<CompoundTypeItem> {
        parse!(Ident)
        .then_ignore(just(op!(":")))
        .then(typ.map(Box::new))
            .map(|( ident, typ )| CompoundTypeItem { ident, typ })
            .map_with_span(map_span).labelled("compound type key-value pair")
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct CompoundType(Vec<S<CompoundTypeItem>>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl CompoundType {
    pub fn parser_inner(typ: S<Type>) -> S<Opt<CompoundType>> {
        parse!(CompoundTypeItem + typ)
            // typ, typ
            .separated_by(just(op!(",")))
                // {typ, typ}
                .delimited_by(just(op!("{")), just(op!("}")))
                .map(CompoundType).labelled("compound type")
                    // error handling
                    .map_with_span(map_ok_span)
                    .recover_with(nested_delimiters(
                             op!("{"), op!("}"),
                        [
                            (op!("("), op!(")")),
                            (op!("["), op!("]")),
                            (op!("<"), op!(">"))
                        ],
                    err_span))
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionType {
      parameters: DataType,
    return_value: Option<Box<S<Type>>>,
}

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl FunctionType {
    pub fn parser_inner(typ: S<Type>) -> S<FunctionType> {
        just(kw!("fn"))
        .ignore_then(
            parse!(DataType + typ.clone())
        ).then(
            just(op!("->"))
            .ignore_then(typ.map(Box::new))
                .or_not()
        )
                .map(|(parameters, return_value)| FunctionType { parameters, return_value })
                .map_with_span(map_span).labelled("function type")
    }
}
