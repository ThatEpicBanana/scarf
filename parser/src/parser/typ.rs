use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
        Path(GenericArgPath),
    Function(FunctionType),
// data
       Tuple(Opt<TupleType>),
        List(Opt<ListType>),
    Compound(Opt<CompoundType>),
}

#[parser_util(derive_parsable)]
impl Type {
    pub fn parser() -> S<Type> {
        recursive(|typ| parse!(Type + typ))
    }

    pub fn parser_inner(typ: S<Type>) -> S<Type> {
        choice((
            parse!(GenericArgPath + typ.clone()).map(Spanned::unspan).map(Type::Path),
              parse!(FunctionType + typ.clone()).map(Spanned::unspan).map(Type::Function),
            // data
                 parse!(TupleType + typ.clone()).map(Type::Tuple),
                  parse!(ListType + typ.clone()).map(Type::List),
              parse!(CompoundType + typ.clone()).map(Type::Compound),
        ))
            .labelled("type")
            .map_with_span(map_span)
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TupleType(Vec<Box<S<Type>>>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl TupleType {
    pub fn parser_inner(typ: S<Type>) -> Opt<TupleType> {
        typ.map(Box::new)
            // typ, typ
            .separated_by(just(op!(",")))
                // (typ, typ)
                .delimited_by(just(op!("(")), just(op!(")")))
                .map(TupleType).labelled("tuple type")
                    // error handling
                    .map(Opt::Ok)
                    .recover_with(nested_delimiters(
                             op!("("), op!(")"),
                        [
                            (op!("["), op!("]")),
                            (op!("<"), op!(">")),
                            (op!("{"), op!("}")),
                        ],
                    |_| Err))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ListType(Box<S<Type>>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl ListType {
    pub fn parser_inner(typ: S<Type>) -> Opt<ListType> {
        typ.map(Box::new)
            // [typ]
            .delimited_by(just(op!("[")), just(op!("]")))
            .map(ListType).labelled("list type")
                // error handling
                .map(Opt::Ok)
                .recover_with(nested_delimiters(
                         op!("["), op!("]"),
                    [
                        (op!("("), op!(")")),
                        (op!("<"), op!(">")),
                        (op!("{"), op!("}")),
                    ],
                |_| Err))
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
    pub fn parser_inner(typ: S<Type>) -> Opt<CompoundType> {
        parse!(CompoundTypeItem + typ)
            // typ, typ
            .separated_by(just(op!(",")))
                // {typ, typ}
                .delimited_by(just(op!("{")), just(op!("}")))
                .map(CompoundType).labelled("compound type")
                    // error handling
                    .map(Opt::Ok)
                    .recover_with(nested_delimiters(
                             op!("{"), op!("}"),
                        [
                            (op!("("), op!(")")),
                            (op!("["), op!("]")),
                            (op!("<"), op!(">"))
                        ],
                    |_| Err))
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DataType {
        Tuple(Opt<   TupleType>),
         List(Opt<    ListType>),
     Compound(Opt<CompoundType>),
}

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl DataType {
    pub fn parser_inner(typ: S<Type>) -> S<DataType> {
        choice((
               parse!(TupleType + typ.clone()).map(DataType::Tuple),
                parse!(ListType + typ.clone()).map(DataType::List),
            parse!(CompoundType + typ        ).map(DataType::Compound),
        ))
            .map_with_span(map_span)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionType {
      parameters: S<DataType>,
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


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::prelude::*;

    #[test]
    fn paths() {
        test_parser(indoc! {r#"
                int;
                generics<int>;
            "#},
            parse!(Type).separated_by(just(op!(";"))).allow_trailing(),
            vec![

            ],
            HashMap::new()
        )
    }
}
