use crate::parser::prelude::*;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type {
        Path(Box<GenericArgPath>), // box is not necessary but i think it reduces the size
    Function(Box<FunctionType>),
// data
       Tuple(Box<Opt<TupleType>>),
        List(Box<Opt<ListType>>),
    Compound(Box<Opt<CompoundType>>),
}

#[parser_util(derive_parsable)]
impl Type {
    pub fn parser() -> S<Type> {
        recursive(|typ| parse!(Type + typ))
    }

    pub fn parser_inner(typ: S<Type>) -> S<Type> {
        choice((
            parse!(GenericArgPath + typ.clone()).map(Spanned::unspan).map(Box::new).map(Type::Path),
              parse!(FunctionType + typ.clone()).map(Spanned::unspan).map(Box::new).map(Type::Function),
            // data
                 parse!(TupleType + typ.clone()).map(Box::new).map(Type::Tuple),
                  parse!(ListType + typ.clone()).map(Box::new).map(Type::List),
              parse!(CompoundType + typ.clone()).map(Box::new).map(Type::Compound),
        ))
            .labelled("type")
            .map_with_span(map_span)
    }

    pub fn path(path: GenericArgPath) -> Type {
        Self::Path(Box::new(path))
    }

    pub fn function(function: FunctionType) -> Type {
        Self::Function(Box::new(function))
    }

    pub fn tuple(tuple: TupleType) -> Type {
        Self::Tuple(Box::new(Ok(tuple)))
    }
    
    pub fn list(list: ListType) -> Type {
        Self::List(Box::new(Ok(list)))
    }

    pub fn compound(compound: CompoundType) -> Type {
        Self::Compound(Box::new(Ok(compound)))
    }
}

impl From<GenericArgPath> for Type {
    fn from(v: GenericArgPath) -> Self {
        Self::path(v)
    }
}

impl From<FunctionType> for Type {
    fn from(v: FunctionType) -> Self {
        Self::function(v)
    }
}

impl From<TupleType> for Type {
    fn from(tuple: TupleType) -> Self {
        Self::tuple(tuple)
    }
}

impl From<ListType> for Type {
    fn from(list: ListType) -> Self {
        Self::list(list)
    }
}

impl From<CompoundType> for Type {
    fn from(compound: CompoundType) -> Self {
        Self::compound(compound)
    }
}


#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TupleType(Vec<S<Type>>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl TupleType {
    pub fn parser_inner(typ: S<Type>) -> Opt<TupleType> {
        typ
            // typ, typ
            .separated_by(just(op!(","))).allow_trailing()
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

    pub fn empty() -> TupleType {
        TupleType(vec![])
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ListType(S<Type>);

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl ListType {
    pub fn parser_inner(typ: S<Type>) -> Opt<ListType> {
        typ
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
    // TODO: allow for string keys
    ident: S<Ident>,
      typ: S<Type>,
}

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl CompoundTypeItem {
    pub fn parser_inner(typ: S<Type>) -> S<CompoundTypeItem> {
        parse!(Ident)
        .then_ignore(just(op!(":")))
        .then(typ)
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
            .separated_by(just(op!(","))).allow_trailing()
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
    return_value: Option<S<Type>>,
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
            .ignore_then(typ)
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
        parser_test!(
            parse!(Type),
            (
                "int",
                span(0..3, Type::path(
                    GenericArgPath {
                        path: Path::parse_offset(0, "int"),
                        generics: None,
                    }
                ))
            ),
            (
                "generics<int, long>",
                span(0..19, Type::path(
                    GenericArgPath {
                        path: Path::parse_offset(0, "generics"),
                        generics: Some(ok_span(8..19, GenericArguments::new(vec![
                            span(9..12, Type::path(
                                GenericArgPath {
                                    path: Path::parse_offset(9, "int"),
                                    generics: None,
                                }
                            )).into(),
                            span(14..18, Type::path(
                                GenericArgPath {
                                    path: Path::parse_offset(14, "long"),
                                    generics: None,
                                }
                            )).into(),
                        ]))),
                    }
                ))
            ),
            (
                "raycaster:foo.bar<int>",
                span(0..22, Type::path(
                    GenericArgPath {
                        path: Path::parse_offset(0, "raycaster:foo.bar"),
                        generics: Some(ok_span(17..22, GenericArguments::new(vec![
                            span(18..21, Type::path(
                                GenericArgPath {
                                    path: Path::parse_offset(18, "int"),
                                    generics: None,
                                }
                            )).into(),
                        ]))),
                    }
                ))
            ),
            (
                "error<error<*>>",
                span(0..15, Type::path(
                    GenericArgPath {
                        path: Path::parse_offset(0, "error"),
                        generics: Some(ok_span(5..15, GenericArguments::new(vec![
                            span(6..14, Type::path(
                                GenericArgPath {
                                    path: Path::parse_offset(6, "error"),
                                    generics: Some(span(11..14, Err))
                                }
                            )).into(),
                        ]))),
                    }
                )),
                {
                    12..13 => Unexpected: op!("*")
                }
            )
        );
    }

    // #[test]
    // fn tuples() {
    //     test_parser(indoc! {r#"
    //             ();
    //             (int);
    //             (int, int,);
    //             (int<(int)>);
    //         "#},
    //         parse!(Type).separated_by(just(op!(";"))).allow_trailing(),
    //         vec![
    //             span(0..2, TupleType::empty().into()),
    //             span(4..9, TupleType(vec![
    //                 GenericArgPath::parse_offset(5, "int").map(Type::from),
    //             ]).into()),
    //         ],
    //         HashMap::new(),
    //     )
    // }
}
