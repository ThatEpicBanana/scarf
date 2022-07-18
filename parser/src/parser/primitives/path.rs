use std::fmt::Display;

use crate::parser::prelude::*;
use pattern::CompoundPattern;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathRoot {
    This,
    Selff,
    Basket,
    Part(PathPart)
}

#[parser_util(derive_parsable)]
impl PathRoot {
    fn parser() -> S<PathRoot> {
          just(kw!("basket")).to(PathRoot::Basket)
        .or(just(kw!("this")).to(PathRoot::This))
        .or(just(kw!("self")).to(PathRoot::Selff))
            .map_with_span(map_span)
        .or(parse!(PathPart).map(|Spanned(spn, part)| map_span(PathRoot::Part(part), spn.unwrap())))
        .labelled("path root")
    }
}

impl Display for PathRoot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            &PathRoot::Part(x) => write!(f, "{}", x),
            &PathRoot::Selff   => write!(f, "self"),
            &PathRoot::This    => write!(f, "this"),
            &PathRoot::Basket  => write!(f, "basket"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathPart {
    Super,
    Id(Ident)
}

#[parser_util(derive_parsable)]
impl PathPart {
    fn parser() -> S<PathPart> {
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

impl Display for PathPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            &PathPart::Super => write!(f, "super"),
            &PathPart::Id(x) => write!(f, "{}", x),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Path {
    pub root: S<PathRoot>,
    pub parts: Vec<S<PathPart>>,
    pub absolute: bool,
}

#[parser_util(derive_parsable)]
impl Path {
    /// Outputs a path of the given root and parts
    pub fn new(root: S<PathRoot>, parts: Vec<S<PathPart>>, absolute: bool) -> Path {
        Path { root, parts, absolute }
    }

    /// Parses into a [`Path`]
    ///
    /// **Examples:**
    /// ```ignore
    /// this.x
    /// ```
    pub fn parser() -> S<Path> {
        // parse the root of the path
        parse!(PathRoot).then(
            // then an optional : to mark as absolute
            just(op!(":")).to(true)
            .or(just(op!(".")).to(false))
                // then the rest of the parts
                .then(parse!(PathPart)
                    .separated_by(just(op!(".")))
                )
            // or a path with just a root
            .or(empty().to((false, vec![])))
        ).labelled("path")
            .map_with_span(map_span)
            .validate(|Spanned(pathspan, (root, ( absolute, parts ))), span, emit| {
                let path = Spanned::new(pathspan, Path { root, parts, absolute });

                // if it's an absolute path
                if absolute {
                    // check if it has an acceptable root
                    match &path.root {
                        Spanned(_, PathRoot::Basket) => (),
                        Spanned(_, PathRoot::Part(PathPart::Id(_))) => (),
                        _ => emit(ParserError::from_reason(span, ParserErrorReason::AbsolutePathDisallowedRoot { path: path.clone() }))
                    }
                }

                path
            })
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
            absolute: false,
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

#[parser_util(derive_parsable,
    defaults(parse!(Type))
)]
impl GenericArgPath {
    /// Creates a new [`GenericPath`] using a spanned [`Path`] and [`GenericArguments`]
    pub fn new(path: S<Path>, generics: Option<S<Opt<GenericArguments>>>) -> Self {
        GenericArgPath { path, generics }
    }

    //ADDDOC
    pub fn parser_inner(typ: S<Type>) -> S<GenericArgPath> {
        parse!(Path)
        .then(parse!(GenericArguments + typ).or_not())
                .map(|(path, generics)| GenericArgPath::new(path, generics))
                .map_with_span(map_span)
    }
}



#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericParamPath {
    path: S<Path>,
    generics: Option<S<Opt<GenericParameters>>>,
}

#[parser_util(derive_parsable)]
impl GenericParamPath {
    /// Creates a new [`GenericPath`] using a spanned [`Path`] and [`GenericParameters`]
    pub fn new(path: S<Path>, generics: Option<S<Opt<GenericParameters>>>) -> Self {
        GenericParamPath { path, generics }
    }

    //ADDDOC
    pub fn parser() -> S<GenericParamPath> {
        parse!(Path)
        .then(parse!(GenericParameters).or_not())
                .map(|(path, generics)| GenericParamPath::new(path, generics))
                .map_with_span(map_span)
    }
}




#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IndexedPath(Vec<S<IndexedPathPart>>);

#[parser_util(derive_parsable,
    defaults(parse!(CompoundPattern))
)]
impl IndexedPath {
    pub fn parser_inner(compound_pattern: CompoundPattern) -> S<IndexedPath> {
        parse!(IndexedPathPart + compound_pattern)
            .separated_by(just(op!("."))).at_least(1)
            .map(IndexedPath)
            .map_with_span(map_span)
            .labelled("indexed path")
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IndexedPathPart {
    name: S<IndexedPathName>,
    modifiers: Vec<S<Opt<IndexedPathModifier>>>,
}

#[parser_util(derive_parsable,
    defaults(parse!(pattern::CompoundPattern))
)]
impl IndexedPathPart {
    pub fn new(name: S<IndexedPathName>, modifiers: Vec<S<Opt<IndexedPathModifier>>>) -> Self {
        IndexedPathPart { name, modifiers }
    }

    pub fn from_name(name: S<IndexedPathName>) -> S<IndexedPathPart> {
        name.into()
    }

    pub fn parser_inner(compound_pattern: CompoundPattern) -> S<IndexedPathPart> {
        parse!(IndexedPathName)
        .then(parse!(IndexedPathModifier + compound_pattern).repeated())
                .map(|(name, modifiers)| IndexedPathPart { name, modifiers })
                .map_with_span(map_span)
                .labelled("indexed path part")
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

#[parser_util(derive_parsable, 
    defaults(parse!(pattern::CompoundPattern))
)]
impl IndexedPathModifier {
    pub fn parser_inner(compound_pattern: CompoundPattern) -> S<Opt<IndexedPathModifier>> {
        // test{pattern}
        compound_pattern.clone()
            .map(IndexedPathModifier::CompoundBound)
            .map_with_span(map_ok_span)
        .or(
                // test[{pattern}]
                compound_pattern
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
        ).labelled("indexed path modifier")
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

#[parser_util(derive_parsable)]
impl IndexedPathName {
    pub fn parser() -> S<IndexedPathName> {
            filter(Token::is_string)
                .map(Token::take_string)
                .map(IndexedPathName::String)
                .map_with_span(map_span)
        .or(
            parse!(PathPart)
                .map(Spanned::unspan)
                .map(IndexedPathName::PathPart)
                .map_with_span(map_span)
        ).labelled("indexed path name")
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::prelude::{*, prelude::pattern::CompoundPattern};

    #[test]
    fn paths() {
        test_parser(indoc! {r#"
                this.a.b;

                self.super.super.foo;

                basket.b;

                thate:raycast.raycaster;

                this;
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
                    ],
                    false
                )),
                // self:super.super.foo;
                span(11..31, Path::new(
                    span(11..15, PathRoot::Selff),
                    vec![
                        span(16..21, PathPart::Super),
                        span(22..27, PathPart::Super),
                        span(28..31, "foo".into()),
                    ],
                    false
                )),
                // basket.b;
                span(34..42, Path::new(
                    span(34..40, PathRoot::Basket),
                    vec![
                        span(41..42, "b".into()),
                    ],
                    false
                )),
                // thate:raycast.raycaster;
                span(45..68, Path::new(
                    span(45..50, PathRoot::Part("thate".into())),
                    vec![
                        span(51..58, "raycast".into()),
                        span(59..68, "raycaster".into()),
                    ],
                    true
                )),
                // this;
                span(71..75, Path::new(
                    span(71..75, PathRoot::This),
                    vec![],
                    false
                )),
            ],
            HashMap::from([])
        )
    }

    #[test]
    fn absolute_path() {
        let path =
            span(0..14, Path::new(
                span(0..5, PathRoot::Part(PathPart::Super)),
                vec![
                    span(6..14, "absolute".into())
                ],
                true
            ));

        test_parser(indoc! {r#"
                super:absolute
            "#},
            parse!(Path),
            path.clone(),
            HashMap::from([
                (0..14, (ParserErrorReason::AbsolutePathDisallowedRoot { path }, None))
            ])
        )
    }

    #[test]
    fn indexed_path() {
        test_parser(indoc! {r#"
                item.tag.Inventory[{Slot @@ 1}].tag."has space"{id @@ 1}.list[0]
            "#},
            parse!(IndexedPath)
                .separated_by(just(op!(";"))).allow_trailing()
                .then_ignore(end()),
            vec![
                span(0..64, IndexedPath(vec![
                    IndexedPathPart::from_name(
                        PathPart::parse_offset(0, "item").into()
                    ),
                    IndexedPathPart::from_name(
                        PathPart::parse_offset(5, "tag").into()
                    ),
                    span(9..31, IndexedPathPart {
                        name: PathPart::parse_offset(9, "Inventory").into(),
                        modifiers: vec![
                            ok_span(18..31, IndexedPathModifier::CompoundIndex(
                                CompoundPattern::parse_offset(19, "{Slot @@ 1}")
                            ))
                        ]
                    }),
                    IndexedPathPart::from_name(
                        PathPart::parse_offset(32, "tag").into()
                    ),
                    span(36..56, IndexedPathPart {
                        name: span(36..47, "has space".to_string()).into(),
                        modifiers: vec![
                            ok_span(47..56, IndexedPathModifier::CompoundBound(
                                CompoundPattern::parse_offset(47, "{id @@ 1}")
                            ))
                        ]
                    }),
                    span(57..64, IndexedPathPart {
                        name: PathPart::parse_offset(57, "list").into(),
                        modifiers: vec![
                            ok_span(61..64, IndexedPathModifier::ExpressionIndex(
                                Expression::parse_offset(62, "0")
                            ))
                        ]
                    })
                ]))
            ],
            HashMap::from([])
        )
    }
}


