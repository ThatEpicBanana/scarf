use crate::parser::prelude::*;

// TODO: other types of paths - import, generic parameters, indexed

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathRoot {
    This,
    Selff,
    Basket,
    Part(PathPart)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathPart {
    Super,
    Id(Ident)
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
        path_root().then(
            // optional : after the root
                just(OP_COLON)
                    .ignore_then(path_part())
                    .or_not()
            .chain( // then repeated . then part
                just(OP_DOT)
                    .ignore_then(path_part())
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

fn path_root() -> impl Parser<Token, S<PathRoot>, Error = Simple<Token>> {
    just(KW_BASKET).to(PathRoot::Basket)
    .or(just(KW_THIS).to(PathRoot::This))
    .or(just(KW_SELF).to(PathRoot::Selff))
        .map_with_span(map_span)
    .or(path_part().map(|Spanned(spn, part)| map_span(PathRoot::Part(part), spn.unwrap())))
    .labelled("path root")
}

fn path_part() -> impl Parser<Token, S<PathPart>, Error = Simple<Token>> {
    just(KW_SUPER).to(PathPart::Super)
        .map_with_span(map_span)
    .or(parse!(Ident).map(|Spanned(spn, id)| map_span(PathPart::Id(id), spn.unwrap())))
    .labelled("path part")
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericPath {
    path: S<Path>,
    generics: Option<S<Opt<GenericArguments>>>,
}

#[derive_parsable]
impl GenericPath {
    /// Creates a new [`GenericPath`] using a spanned [`Path`] and [`GenericArguments`]
    pub fn new(path: S<Path>, generics: Option<S<Opt<GenericArguments>>>) -> Self {
        GenericPath { path, generics }
    }

    //ADDDOC
    pub fn parser() -> impl Parser<Token, S<GenericPath>, Error = Simple<Token>> {
        parse!(Path)
        .then(parse!(GenericArguments).or_not())
                .map(|(path, generics)| GenericPath::new(path, generics))
                .map_with_span(map_span)
    }
}



#[test]
#[cfg(test)]
fn paths() {
    use crate::tests::prelude::*;
    use path::*;

    test_parser(indoc! {r#"
            this.a.b;

            self:super.super.foo;

            basket.b;

            thate:raycast.raycaster;
        "#},
        parse!(Path)
            .separated_by(just(OP_SEMI)).allow_trailing()
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
