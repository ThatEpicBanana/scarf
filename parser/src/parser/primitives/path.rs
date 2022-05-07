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
    /// # Panics
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
    /// # Panics
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

//TODO: convert this to using an actual parser
impl From<&str> for Path {
    /// Converts a string into a path
    /// 
    /// # Panics
    /// 
    /// - If the string is empty
    /// - If the string has more than one colon
    fn from(string: &str) -> Path {
        let mut list: Vec<_> = string.split(":").collect();

        // handle optional colon
        let mut list: Box<dyn Iterator<Item = &str>> = match list.len() {
            0 => panic!("String being converted into path is empty!"),
            1 => {
                Box::new(
                    list.pop()
                        .unwrap()
                        .split(".")
                )
            }, 
            2 => {
                Box::new(
                    iter::once(
                        list.pop() // index 0
                            .unwrap()
                    ).chain(
                        list.pop() // index 1
                            .unwrap()
                            .split(".")
                    )
                )
            },
            3.. => panic!("String being converted into path has more than one colon (:)!"),
            _ => panic!("String length returned an illegal number somehow")
        };

        
        // get root
        let root = match list.next() {
            Some("this") => PathRoot::This,
            Some("basket") => PathRoot::Basket,
            Some(x) => PathRoot::Part(string_to_path_part(x)),
            None => panic!(), // shouldn't be possible
        };
        
        let parts = list
            .map(string_to_path_part)
            .map(no_span)
            .collect();

        // return
        Path { root: no_span(root), parts }
    }
}

fn path_root() -> impl Parser<Token, S<PathRoot>, Error = Simple<Token>> {
    just(KW_BASKET).to(PathRoot::Basket)
    .or(just(KW_THIS).to(PathRoot::This))
        .map_with_span(span)
    .or(path_part().map(|Spanned(part, spn)| span(PathRoot::Part(part), spn.unwrap())))
    .labelled("path root")
}

fn path_part() -> impl Parser<Token, S<PathPart>, Error = Simple<Token>> {
    just(KW_SUPER).to(PathPart::Super)
    .or(just(KW_SELF).to(PathPart::Selff))
        .map_with_span(span)
    .or(ident::ident().map(|Spanned(idt, spn)| span(PathPart::Id(idt), spn.unwrap())))
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
    .map_with_span(span)
}