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
    pub root: PathRoot,
    pub parts: Vec<PathPart>,
}

impl Path {
    /// Outputs a path of the given root and parts
    pub fn new(root: PathRoot, parts: Vec<PathPart>) -> Path {
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
            root: PathRoot::Part(parts.next().expect("List given must have at least one element!")),
            parts: parts.collect(),
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
        let mut parts = parts.into_iter();

        Path { 
            root: PathRoot::Part(parts.next().expect("List given must have at least one element!")),
            parts: parts.collect(),
        }
    }
}

fn string_to_path_part(string: &str) -> PathPart {
    match string {
        "super" => PathPart::Super,
        "self" => PathPart::Selff,
        x => PathPart::Id(x.into()),
    }
}

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

        let mut parts = vec![];

        // get root
        let root = match list.next() {
            Some("this") => PathRoot::This,
            Some("basket") => PathRoot::Basket,
            Some(x) => PathRoot::Part(string_to_path_part(x)),
            None => panic!(), // shouldn't be possible
        };

        // convert parts to PathParts
        for part in list {
            parts.push(string_to_path_part(part));
        }

        // return
        Path { root, parts }
    }
}

fn path_part() -> impl Parser<Token, PathPart, Error = Simple<Token>> {
    just(KW_SUPER).to(PathPart::Super)
        .or(just(KW_SELF).to(PathPart::Selff))
        .or(ident::ident().map(|idt| PathPart::Id(idt)))
}

pub fn path() -> impl Parser<Token, Path, Error = Simple<Token>> {
    // root
    just(KW_BASKET).to(PathRoot::Basket)
        .or(just(KW_THIS).to(PathRoot::This))
        .or(path_part().map(PathRoot::Part))
    .then(
        // optional : after the root
            just(OP_COLON)
                .ignore_then(path_part())
                .or_not()
        .chain( // then repeated . then part
            just(OP_DOT)
                .ignore_then(path_part())
                .repeated()
        )
    ).map(|(root, parts)| Path{root, parts}) // map to path
}



// #[cfg(test)]
// mod tests {
//     use super::*;
//     use crate::prelude::*;
// }