// i'm sorry
#![allow(non_snake_case)]
#![allow(non_camel_case_types)]
#![allow(unused_variables)]
#![allow(non_upper_case_globals)]

// macro for constructing the keywords
macro_rules! kw_macro {
    { $( $name:ident = $match:expr),* $(,)? } => {
        use std::collections::HashMap;

        #[allow(non_camel_case_types)]
        #[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
        pub enum Keyword {
            $($name),*
        }

        impl std::fmt::Display for Keyword {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match *self {
                    $( 
                        Keyword::$name => write!(f, "{}", $match), 
                    )*
                }
            }
        }

        lazy_static::lazy_static!{
            pub static ref keywords: HashMap<String, Keyword> = {
                let mut set = HashMap::new();
                $( set.insert($match.to_string(), Keyword::$name); )*
                set
            };
        }

        pub mod list {
            use crate::lexer::prelude::*;

            $( pub const $name: Token = KEYWORD(super::Keyword::$name); )*
        }
    };
}

// - actual definitions -
kw_macro!{
    // various types
    KW_LET = "let",
    KW_DATA = "data",
    KW_SCORE = "score",
    KW_ENTITY = "entity",
    KW_BLOCK = "block",
    // items
    KW_MOD = "mod",
    KW_CLASS = "class", KW_EXTENDS = "extends",
    KW_ENUM = "enum",
    KW_IMPORT = "import",
    KW_MACRO = "macro", KW_PROC_MACRO = "proc_macro",
    KW_TYPE = "type",
    KW_FUNC = "func",
    KW_GET = "get", KW_SET = "set", // properties

    // statements
    KW_BREAK = "break",
    KW_CONTINUE = "continue",
    KW_RETURN = "return",

    // suffixes
    // could be removed
    // KW_ID = "id", KW_POS = "pos", KW_SEL = "sel", KW_DAT = "dat", KW_BRD = "brd", KW_CMD = "cmd", // string
    // KW_B = "b", KW_I = "i", KW_S = "s", KW_L = "l", // integer
    // KW_F = "f", KW_ANG = "ang", // float

    // expressions
    KW_NEW = "new", KW_SUMMON = "summon", KW_INST = "inst", // instantiating
    KW_AS = "as", KW_IMPL = "impl", KW_INIT = "init", // casting
    KW_LOOP = "loop", KW_WHILE = "while", KW_FOR = "for", KW_IN = "in", // loops
    KW_IF = "if", KW_ELSE = "else", 
    KW_MATCH = "match",
    KW_TRY = "try", KW_CATCH = "catch",
    //   execute
    KW_ALIGN = "align", KW_ANCHORED = "anchored", // in
    KW_AT = "at", KW_LIKE = "like", // as
    KW_FACING = "facing", 
    KW_POSITIONED = "positioned", KW_ROTATED = "rotated", 
    KW_EYES = "eyes", KW_FEET = "feet",

    // visibility
    KW_PUB = "pub",
    KW_PRV = "prv",

    // paths
    KW_SUPER = "super",
    KW_SELF = "self",
    KW_BASKET = "basket",
    KW_THIS = "this",
}