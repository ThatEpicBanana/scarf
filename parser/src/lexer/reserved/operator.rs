// i'm sorry
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_variables)]
#![allow(non_upper_case_globals)]

macro_rules! op_macro {
    {
        $(
            $group:ident {
                $($name:ident = $match:expr),* $(,)?
            }
        )*
    } => {
        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum Operator { 
            #[deprecated = "only (should be) used internally for a macro"]
            Empty,
            $($($name),*),*
        }

        impl std::fmt::Display for Operator {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match *self {
                    $($(
                        Operator::$name => write!(f, "{}", $match),
                    )*)*
                    #[allow(deprecated)]
                    Operator::Empty => write!(f, "OP_EMPTY")
                }
            }
        }

        pub mod any_match {
            use chumsky::prelude::*;
            use super::Operator::{self, *};

            $(
                pub fn $group() -> impl Parser<char, Operator, Error = Simple<char>> { 
                    choice((
                        $(
                            just($match).to($name)
                        ),*
                    ))
                }
            )*
        }

        pub mod lists {
            use super::Operator::{self, *};
            use std::collections::HashSet;

            lazy_static::lazy_static!{$(
                pub static ref $group: HashSet<Operator> = {
                    let mut set = HashSet::new();
                    $( set.insert($name); )*
                    set
                };
            )*}
        }

        pub mod any_token {
            use chumsky::prelude::*;
            use crate::lexer::prelude::*;

            $(
                pub fn $group() -> impl Parser<Token, Token, Error = Simple<Token>> { 
                    filter(|tok| 
                        if let OPERATOR{op, ..} = tok {
                            super::lists::$group.contains(op)
                        } else { false }
                    )
                }
            )*
        }

        pub mod list {
            use crate::lexer::prelude::*;
            $($(
                pub const $name: Token = OPERATOR{
                    op: super::Operator::$name, 
                    assignment: false
                };
            )*)*
        }

        use chumsky::prelude::*;
        pub fn any_match() -> impl Parser<char, Operator, Error = Simple<char>> {
            #[allow(deprecated)]
            empty().to(Operator::Empty)$(
                .or(any_match::$group())
            )*
        }
    }
}

// // - actual definitions -
op_macro!{
    top { 
        // item
        OP_EQUAL_ARROW = "=>", 
        OP_RETURN = "->",
        OP_HASH = "#", OP_EXCLAMATION = "!",

        // comparison
        OP_EQUAL = "=",
        OP_DOUBLE_EQUAL = "==", OP_FUZZY_EQUAL = "~=", OP_NOT_EQUAL = "!=", OP_FUZZY_NOT_EQUAL = "!~=",
        OP_LESS = "<", OP_MORE = ">", OP_LESS_EQUAL = "<=", OP_MORE_EQUAL = ">=",
    } 

    bottom {
        // other / in multiple
        OP_COLON = ":", OP_DOUBLE_COLON = "::",
        OP_DOUBLE_DOT = "..", OP_TRIPLE_DOT = "...",
        OP_BAR = "|",
        OP_AT = "@", OP_BACKTICK = "`",
    
        // seperations
        OP_SEMI = ";",
        OP_COMM = ",",
        OP_DOT = ".",
    } 

    grouping {
        // brackets
        OP_LCURLY  = "{", OP_RCURLY  = "}",
        OP_LPARA   = "(", OP_RPARA   = ")",
        OP_LSQUARE = "[", OP_RSQUARE = "]",
        OP_LANGLE  = "<", OP_RANGLE  = ">",
    }

    assign {
        // operator expression
        //   multiple
        OP_DOUBLE_PLUS = "++", OP_DOUBLE_MINUS = "--",
        OP_PLUS = "+", OP_MINUS = "-",
        OP_STAR = "*",
    
        //   prefix
        OP_AND = "&", OP_NOT = "!",
    
        //   binary
        OP_UP_ARROW = "^",
        OP_SLASH = "/", OP_MODULO = "%",
        OP_RANGE_OPEN = "..<", // OP_DOUBLE_DOT
        OP_DOUBLE_AND = "&&", 
        OP_OR = "||",
    
        // pattern
        OP_WILDCARD = "_",
    }
}