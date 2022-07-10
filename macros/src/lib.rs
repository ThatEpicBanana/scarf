mod parser;

extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{self, *};

/// Various utilites for creating parsers, bundled together to make sure they're in the correct order.
///
/// # Syntax
///
/// ```ignore
/// #[parser_util(defaults(...), no_convert, derive_parsable)]
/// ```
/// &nbsp;
/// # Features
///
/// ## Converts inputs and outputs into parsers
///
/// ```ignore
/// pub fn parser(expr: S<Expression>) -> S<SinglePattern>
///
/// // Converts to
///
/// pub fn parser(expr: impl Parser<Token, S<Expression>, Error = Simple<Token>>)
///     -> impl Parser<Token, S<SinglePattern>, Error = Simple<Token>>
/// ```
/// &nbsp;
/// This is on by default, but can be turned off by `no_convert`
///
/// ### Choosing some inputs to not be converted
///
/// Adding `#[no_convert]` before an argument marks it to not be converted into a parser.
///
/// ```ignore
/// pub fn parser(expr: S<Expression>, #[no_convert] type_marker: Token) -> S<IdentifierInfo>
/// ```
/// &nbsp;
/// You may also add `#[no_convert]` before the entire parser to do the same for the return type
///
/// ### Converting other functions
///
/// You may add `#[parser]` to other functions to mark them as a parser to convert
///
/// ```ignore
/// #[parser]
/// fn pattern_list(single_pattern: Box<S<SinglePattern>>) -> Vec<Box<S<SinglePattern>>> {...}
/// ```
///
/// ## Defaults for parser_inner
///
/// Optionally automatically creates a `parser` function that uses the defaults specified
///
/// ```ignore
/// #[parser_util(defaults(parse!(SinglePattern)))]
/// impl Pattern {
///     pub fn parser_inner(single_pattern: S<SinglePattern>) -> S<Pattern> {...}
/// }
///
/// // Converts to
///
/// impl Pattern {
///     pub fn parser_inner(single_pattern: S<SinglePattern>) -> S<Pattern> {...}
///
///     pub fn parser() -> S<Pattern> {
///         Self::parser_inner(parse!(SinglePattern))
///     }
/// }
/// ```
/// &nbsp;
/// ## Derives Parsable
///
/// Disable via `no_derive_parsable`
///
/// See [`derive_parsable`]
#[proc_macro_attribute]
pub fn parser_util(args: TokenStream, item: TokenStream) -> TokenStream {
    use punctuated::Punctuated;

    let args: Punctuated<Expr, Token![,]> = parse_macro_input!(args with Punctuated::parse_terminated);

    let mut defaults: Option<Punctuated<Expr, Token![,]>> = None;
    let mut convert = true;
    let mut derive_parsable = false;

    for expr in args {
        match expr {
            Expr::Path(ExprPath{ path, .. }) => {
                if path.is_ident("no_convert") { convert = false; }
                else if path.is_ident("derive_parsable") { derive_parsable = true; }
                else { panic!("Single ident argument `{:?}` does not match `no_convert` or `derive_parsable`", path); }
            },
            Expr::Call(ExprCall{ func, args, .. }) => {
                if let Expr::Path(ExprPath{ path, .. }) = *func {
                    if path.is_ident("defaults") { defaults = Some( args ) }
                    else { panic!("Ident with arguments' path `{:?}` does not match `defaults`", path) }
                } else { panic!("Ident with arguments `{:?}` does not match `defaults(...)`", func); }
            },
            _ => panic!("Inputted argument isn't a singular ident or ident with arguments")
        }
    }

    let item_ast: syn::ItemImpl = syn::parse(item.clone()).expect("Failed parsing impl item");

    parser::parser_util(item_ast, convert, derive_parsable, defaults)
}

/// Converts a function as per [`parser_util`].
///
/// ### Note:
///
/// If used in an impl block, the impl block must have #\[[`parser_util`]\] specified
///
/// ### Always adding lifetimes
///
/// To always add lifetimes to the parsers, use #\[[`parser_fn`]\(always_lifetime\)\]
#[proc_macro_attribute]
pub fn parser_fn(args: TokenStream, item: TokenStream) -> TokenStream {
    let always_lifetime = if ! args.is_empty() {
        let args: syn::Path = syn::parse(args).expect("Failed parsing arguments");
        args.is_ident("always_lifetime")
    } else { false };

    let fn_ast: ItemFn = syn::parse(item).expect("Failed parsing function");

    parser::convert_fn(always_lifetime, fn_ast)
}

/// Converts a type into a function
///
/// `parser!(\*typ\*)`
///
/// > =>
///
/// `impl Parser<Token, *typ*, Error = Simple<Token>>`
#[proc_macro]
pub fn parser(item: TokenStream) -> TokenStream {
    // let lifetime = if ! args.is_empty() {
    //     let args: syn::Path = syn::parse(args).expect("Failed parsing arguments");
    //     args.is_ident("lifetime")
    // } else { false };

    let typ: syn::Type = syn::parse(item).expect("Failed parsing type");

    parser::convert_type(&typ, false).into()
}
