use crate::parser::prelude::*;

// TODO: other types of paths - import, generic parameters, indexed

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathRoot {
    This,
    Selff,
    Basket,
    Part(PathPart)
}

#[derive_parsable]
impl PathRoot {
    fn parser() -> impl Parser<Token, S<PathRoot>, Error = Simple<Token>> {
          just(kw!("basket")).to(PathRoot::Basket)
        .or(just(kw!("this")).to(PathRoot::This))
        .or(just(kw!("self")).to(PathRoot::Selff))
            .map_with_span(map_span)
        .or(parse!(PathPart).map(|Spanned(spn, part)| map_span(PathRoot::Part(part), spn.unwrap())))
        .labelled("path root")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathPart {
    Super,
    Id(Ident)
}

#[derive_parsable]
impl PathPart {
    fn parser() -> impl Parser<Token, S<PathPart>, Error = Simple<Token>> {
        just(kw!("super")).to(PathPart::Super)
            .map_with_span(map_span)
        .or(parse!(Ident).map(|Spanned(spn, id)| map_span(PathPart::Id(id), spn.unwrap())))
        .labelled("path part")
    }
}

impl From<&str> for PathPart {
    fn from(string: &str) -> PathPart {
        PathPart::Id(Ident::from(string))
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub root: S<PathRoot>,
    pub parts: Vec<S<PathPart>>,
}

#[derive_parsable]
impl Path {
    /// Outputs a path of the given root and parts
    pub fn new(root: S<PathRoot>, parts: Vec<S<PathPart>>) -> Path {
        Path { root, parts }
    }

    /// Parses into a [`Path`]
    ///
    /// **Examples:**
    /// ```ignore
    /// this.x
    /// ```
    pub fn parser() -> impl Parser<Token, S<Path>, Error = Simple<Token>> {
        parse!(PathRoot).then(
            // optional : after the root
                just(op!(":"))
                    .ignore_then(parse!(PathPart))
                    .or_not()
            .chain( // then repeated . then part
                just(op!("."))
                    .ignore_then(parse!(PathPart))
                    .repeated()
            )
        ).labelled("path")
        .map(|(root, parts)| Path{root, parts}) // map to path
        .map_with_span(map_span)
    }

    /// Turns a vector of parts into a path, with the first part as the root
    ///
    /// ### Panics
    ///
    /// - If the list has less than one element
    pub fn parts(parts: Vec<PathPart>) -> Path {
        let mut parts = parts.into_iter();

        Path {
            root: no_span(PathRoot::Part(parts.next().expect("List given must have at least one element!"))),
            parts: parts.map(no_span).collect(),
        }
    }
}

impl From<Vec<PathPart>> for Path {
    /// Turns a vector of parts into a path, with the first part as the root
    ///
    /// ### Panics
    /// 
    /// - If the list has less than one element
    fn from(parts: Vec<PathPart>) -> Path {
        Path::parts(parts)
    }
}

fn string_to_path_part(string: &str) -> PathPart {
    match string {
        "super" => PathPart::Super,
        x => PathPart::Id(x.into()),
    }
}





#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericArgPath {
    path: S<Path>,
    generics: Option<S<Opt<GenericArguments>>>,
}

#[derive_parsable]
impl GenericArgPath {
    /// Creates a new [`GenericPath`] using a spanned [`Path`] and [`GenericArguments`]
    pub fn new(path: S<Path>, generics: Option<S<Opt<GenericArguments>>>) -> Self {
        GenericArgPath { path, generics }
    }

    //ADDDOC
    pub fn parser() -> impl Parser<Token, S<GenericArgPath>, Error = Simple<Token>> {
        parse!(Path)
        .then(parse!(GenericArguments).or_not())
                .map(|(path, generics)| GenericArgPath::new(path, generics))
                .map_with_span(map_span)
    }
}



#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericParamPath {
    path: S<Path>,
    generics: Option<S<Opt<GenericParameters>>>,
}

#[derive_parsable]
impl GenericParamPath {
    /// Creates a new [`GenericPath`] using a spanned [`Path`] and [`GenericParameters`]
    pub fn new(path: S<Path>, generics: Option<S<Opt<GenericParameters>>>) -> Self {
        GenericParamPath { path, generics }
    }

    //ADDDOC
    pub fn parser() -> impl Parser<Token, S<GenericParamPath>, Error = Simple<Token>> {
        parse!(Path)
        .then(parse!(GenericParameters).or_not())
                .map(|(path, generics)| GenericParamPath::new(path, generics))
                .map_with_span(map_span)
    }
}




#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IndexedPath(Vec<S<IndexedPathPart>>);

#[derive_parsable]
impl IndexedPath {
    pub fn parser() -> impl Parser<Token, S<IndexedPath>, Error = Simple<Token>> {
        parse!(IndexedPathPart)
            .separated_by(just(op!(".")))
            .map(IndexedPath)
            .map_with_span(map_span)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IndexedPathPart {
    name: S<IndexedPathName>,
    modifiers: Vec<S<Opt<IndexedPathModifier>>>,
}

#[derive_parsable]
impl IndexedPathPart {
    pub fn new(name: S<IndexedPathName>, modifiers: Vec<S<Opt<IndexedPathModifier>>>) -> Self {
        IndexedPathPart { name, modifiers }
    }

    pub fn from_name(name: S<IndexedPathName>) -> S<IndexedPathPart> {
        name.into()
    }

    pub fn parser() -> impl Parser<Token, S<IndexedPathPart>, Error = Simple<Token>> {
        parse!(IndexedPathName)
        .then(parse!(IndexedPathModifier).repeated())
                .map(|(name, modifiers)| IndexedPathPart { name, modifiers })
                .map_with_span(map_span)
    }
}

impl From<S<IndexedPathName>> for S<IndexedPathPart> {
    fn from(name: S<IndexedPathName>) -> Self {
        match name {
            Spanned(spn, name) => Spanned(spn.clone(), IndexedPathPart{
                name: Spanned(spn, name), 
                modifiers: vec![] 
            }),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IndexedPathModifier {
    ExpressionIndex ( S<Expression>            ),
      CompoundIndex ( pattern::CompoundPattern ),
      CompoundBound ( pattern::CompoundPattern ),
}

#[derive_parsable]
impl IndexedPathModifier {
    pub fn parser() -> impl Parser<Token, S<Opt<IndexedPathModifier>>, Error = Simple<Token>> {
        // test{pattern}
        parse!(pattern::CompoundPattern)
            .map(IndexedPathModifier::CompoundBound)
            .map_with_span(map_ok_span)
        .or(
                // test[{pattern}]
                parse!(pattern::CompoundPattern)
                    .map(IndexedPathModifier::CompoundIndex)
            .or(
                // test[expr]
                none_of(op!("{")).rewind()
                .ignore_then(parse!(Expression))
                    .map(IndexedPathModifier::ExpressionIndex)
            )
                // add delimiters
                .delimited_by(just(op!("[")), just(op!("]")))
                .map_with_span(map_ok_span).recover_with(nested_delimiters(op!("["), op!("]"), [], err_span)),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum IndexedPathName {
    PathPart(PathPart),
    String(String),
}

impl From<S<String>> for S<IndexedPathName> {
    fn from(string: S<String>) -> Self {
        match string {
            Spanned(spn, val) => Spanned(spn, IndexedPathName::String(val)),
        }
    }
}

impl From<S<PathPart>> for S<IndexedPathName> {
    fn from(part: S<PathPart>) -> Self {
        match part {
            Spanned(spn, val) => Spanned(spn, IndexedPathName::PathPart(val)),
        }
    }
}

#[derive_parsable]
impl IndexedPathName {
    pub fn parser() -> impl Parser<Token, S<IndexedPathName>, Error = Simple<Token>> {
            filter(Token::is_string)
                .map(Token::take_string)
                .map(IndexedPathName::String)
                .map_with_span(map_span)
        .or(
            parse!(PathPart)
                .map(Spanned::unspan)
                .map(IndexedPathName::PathPart)
                .map_with_span(map_span)
        )
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::prelude::*;

    #[test]
    fn paths() {
        test_parser(indoc! {r#"
                this.a.b;

                self:super.super.foo;

                basket.b;

                thate:raycast.raycaster;
            "#},
            parse!(Path)
                .separated_by(just(op!(";"))).allow_trailing()
                .then_ignore(end()),
            vec![
                // this.a.b;
                span(0..8, Path::new(
                    span(0..4, PathRoot::This),
                    vec![
                        span(5..6, "a".into()),
                        span(7..8, "b".into())
                    ]
                )),
                // self:super.super.foo;
                span(11..31, Path::new(
                    span(11..15, PathRoot::Selff),
                    vec![
                        span(16..21, PathPart::Super),
                        span(22..27, PathPart::Super),
                        span(28..31, "foo".into()),
                    ]
                )),
                // basket.b;
                span(34..42, Path::new(
                    span(34..40, PathRoot::Basket),
                    vec![
                        span(41..42, "b".into()),
                    ]
                )),
                // thate:raycast.raycaster;
                span(45..68, Path::new(
                    span(45..50, PathRoot::Part("thate".into())),
                    vec![
                        span(51..58, "raycast".into()),
                        span(59..68, "raycaster".into()),
                    ]
                ))
            ],
            HashMap::from([])
        )
    }

    #[test]
    fn indexed_path() {
        test_parser(indoc! {r#"
                item.tag.Inventory[{Slot @ 1}].tag."has space"{id @ 1}.list[0]
            "#},
            parse!(IndexedPath)
                .separated_by(just(op!(";"))).allow_trailing()
                .then_ignore(end()),
            vec![
                span(0..62, IndexedPath(vec![
                    IndexedPathPart::from_name(
                        PathPart::parse_offset(0, "item").into()
                    )
                ]))
            ],
            HashMap::from([])
        )
    }
}


