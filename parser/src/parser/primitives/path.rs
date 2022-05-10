use chumsky::Stream;

use crate::prelude::*;
use std::iter;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathRoot {
    This,
    Basket,
    Part(PathPart)
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PathPart {
    Super,
    Selff,
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

impl Path {
    /// Outputs a path of the given root and parts
    pub fn new(root: S<PathRoot>, parts: Vec<S<PathPart>>) -> Path {
        Path { root, parts }
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

    /// Converts a string into a path with spans offset by a set amount
    /// 
    /// This was mainly made for tests, but it might be useful elsewhere.
    /// 
    /// ### Example:
    /// 
    /// ```
    /// # use crate::parser::prelude::*;
    /// # use path::*;
    /// assert_eq!(
    ///     Path::from_offset_string("this.x", 10), 
    ///     Path::new(
    ///         span(PathRoot::This, 10..14), 
    ///         vec![span(PathPart::Id(Ident::new("x")), 15..16)]
    ///     )
    /// );
    /// ```
    /// 
    /// ### Panics:
    /// 
    /// The same as `<Path as From<&str>>::from`
    pub fn from_offset_string(offset: usize, string: &str) -> S<Path> {
        let string: String = 
            iter::repeat(' ').take(offset) // add `offset` spaces
            .chain(string.chars()).collect();   // to start of string

        string.as_str().into()
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
        "self" => PathPart::Selff,
        x => PathPart::Id(x.into()),
    }
}

impl From<&str> for S<Path> {
    /// Converts a string into a path
    /// 
    /// ### Panics
    /// 
    /// - If the lexer or parser fails
    ///     - If there is more than one colon
    ///     - If there any tokens other than identifiers, `:`s, or `.`s
    ///     - If operators are doubled up
    ///     - etc
    fn from(string: &str) -> S<Path> {
        let len = string.len();

        path().parse(Stream::from_iter(len..len+1,
            crate::lexer::create().parse(
                string
            ).expect("Failed to lex path!").into_iter()
        )).expect("Failed to parse path!")
    }
}

fn path_root() -> impl Parser<Token, S<PathRoot>, Error = Simple<Token>> {
    just(KW_BASKET).to(PathRoot::Basket)
    .or(just(KW_THIS).to(PathRoot::This))
        .map_with_span(map_span)
    .or(path_part().map(|Spanned(part, spn)| map_span(PathRoot::Part(part), spn.unwrap())))
    .labelled("path root")
}

fn path_part() -> impl Parser<Token, S<PathPart>, Error = Simple<Token>> {
    just(KW_SUPER).to(PathPart::Super)
    .or(just(KW_SELF).to(PathPart::Selff))
        .map_with_span(map_span)
    .or(ident::ident().map(|Spanned(idt, spn)| map_span(PathPart::Id(idt), spn.unwrap())))
    .labelled("path part")
}

//TODO: check if this actually works
/// Parses into a [`Path`]
/// 
/// **Examples:**
/// ```
/// # use crate::parser::prelude::*;
/// # use path::*;
/// # use crate::tests::test_parser;
/// # 
/// # test_parser(path(), "
/// this.x
/// # ", Path::new(span(PathRoot::This, 1..5), vec![span(PathPart::Id("x".into()), 6..7)]))
/// ```
pub fn path() -> impl Parser<Token, S<Path>, Error = Simple<Token>> {
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct GenericPath {
    path: S<Path>,
    generics: Option<S<Opt<GenericArguments>>>,
}

impl GenericPath {
    /// Creates a new [`GenericPath`] using a spanned [`Path`] and [`GenericArguments`]
    pub fn new(path: S<Path>, generics: Option<S<Opt<GenericArguments>>>) -> Self {
        GenericPath { path, generics }
    }

    //ADDDOC
    pub fn parser() -> impl Parser<Token, S<GenericPath>, Error = Simple<Token>> {
        path::path()
        .then(GenericArguments::parser().or_not())
                .map(|(path, generics)| GenericPath::new(path, generics))
                .map_with_span(map_span)
    }
}

