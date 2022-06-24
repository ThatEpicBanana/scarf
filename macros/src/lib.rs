extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::{self, *};

fn get_return_type_from_parser_func(parser_func: &ImplItemMethod) -> &Type {
    // slowly go down the tree until we get to the type
    match &parser_func.sig.output {
        // return type
        ReturnType::Type(_, return_type) =>
            // return type with bounds
            match &**return_type {
                Type::ImplTrait(TypeImplTrait {
                    bounds, ..
                }) => {
                    let mut iter = bounds.iter();
                    // for each bound
                    loop {
                        let bound = iter.next();
                        match bound {
                            Some(bound) => match &bound {
                                // get path segments
                                &TypeParamBound::Trait(TraitBound {
                                    path: Path { segments, .. }, ..
                                }) => {
                                    // get the last segment
                                    let last = segments.last().unwrap();

                                    // if it is (most likely) a Parser,
                                    if last.ident == Ident::new("Parser", Span::call_site()) {
                                        // get the arguments
                                        match &&last.arguments {
                                            &PathArguments::AngleBracketed(AngleBracketedGenericArguments{
                                                args, ..
                                            }) => {
                                                if args.len() < 2 { panic!("The parser return value does not have enough arguments") }

                                                // get the second argument
                                                match &args[1] {
                                                    GenericArgument::Type(typ) => break typ,
                                                    _ => panic!("Second argument in the parser return value is not a type")
                                                }
                                            },
                                            _ => unreachable!()
                                        }
                                    }
                                },
                                _ => (),
                            },
                            // or panic if there are no more
                            None => panic!("Parser function does not return a Parser"),
                        }
                    }
                },
                _ => panic!("Parser function does not have an impl return type"), 
            }
        _ => panic!("Parser function does not have a return type"),
    }
}

fn impl_method_matches(impl_item: &ImplItem, string: &str) -> bool {
    if let ImplItem::Method(impl_item) = impl_item {
        impl_item.sig.ident.to_string() == string
    } else { false }
}

fn get_impl_function<'a>(item_impl: &'a ItemImpl, string: &str) -> Option<&'a ImplItemMethod> {
    match item_impl.items.iter()
        .find(|val| match val {
            &ImplItem::Method(ImplItemMethod {
                sig: Signature {
                    ident, ..
                }, ..
            }) => ident.to_string() == string,
            _ => false,
        }) 
    {
        Some(ImplItem::Method(x)) => Some(x),
        Some(_) => unreachable!(),
        None => None,
    }
}

/// Implies `Parsable` and [`From<&str>`] using a given `parser` function.
/// 
/// **Note:** The output doesn't have to be the same as the type that will implement `Parsable.`
/// 
/// ### Examples:
///
/// ```ignore
/// /// A struct that holds some extra information about a string
/// #[derive(Clone, PartialEq, Eq, Hash)]
/// pub struct Ident(String);
/// 
/// #[derive_parsable]
/// impl Ident {
///     pub fn new(id: String) -> Ident {
///         Ident(id)
///     }
///
///     pub fn parser() -> impl Parser<Token, S<Ident>, Error = Simple<Token>> {
///         filter(Token::is_ident)
///             .labelled("ident")
///             .map(|tok| tok.into())
///             .map_with_span(map_span)
///     }
/// }
/// ```
#[proc_macro_attribute]
pub fn derive_parsable(_: TokenStream, item: TokenStream) -> TokenStream {
    // let item_ast: Result<syn::ItemImpl> = syn::parse(item.clone());

    // if matches!(item_ast, Err(_)) { println!("{:#?}", syn::parse::<syn::Item>(item.clone())); }

    let item_ast: syn::ItemImpl = syn::parse(item.clone()).expect("Failed parsing impl item");

    // get parent type from impl
    let typ = &*item_ast.self_ty;

    // get first function in the impl block with the identifier of `parser`
    let parser_func = get_impl_function(&item_ast, "parser").expect("No Parser function found");

    let return_type = get_return_type_from_parser_func(parser_func);

    let mut out = item.clone();

    out.extend(TokenStream::from(quote! {
        #[automatically_derived]
        impl Parsable<#return_type> for #typ {
            fn parse(string: &str) -> #return_type {
                let len = string.len();

                #typ::parser().parse(chumsky::Stream::from_iter(len..len+1,
                    crate::lexer::create().parse(
                        string
                    ).expect(concat!("Failed to lex {}", stringify!(#typ))).into_iter()
                )).expect(concat!("Failed to parse {}", stringify!(#typ)))
            }
        }

        #[automatically_derived]
        impl From<&str> for #return_type {
            /// Converts a string into a [`#typ`]
            ///
            /// ### Panics
            /// 
            /// - If the lexer or parser fails
            fn from(string: &str) -> Self {
                #typ::parse(string)
            }
        }
    }));

    out
}

/// Various utilites for creating parsers, bundled together to make sure they're in the correct order.
///
/// # Syntax
///
/// ```ignore
/// #[parser_util(defaults(...), no_convert, no_derive_parsable)]
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
/// pub fn parser(expr: impl Parser<Token, S<Expression>, Error = ParserError>)
///     -> impl Parser<Token, S<SinglePattern>, Error = ParserError>
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

    // get parent type from impl
    let parent_type = &*item_ast.self_ty;

    // get functions from impl
    let mut items = item_ast.items.clone();

    // remove functions from item list as they're found
    let parser_func = items.iter()
        .position(|method| impl_method_matches(method, "parser"))
        .map(|index| if let ImplItem::Method(x) = items.swap_remove(index)
            { x } else { unreachable!() });

    let parser_inner_func = items.iter()
        .position(|method| impl_method_matches(method, "parser_inner"))
        .map(|index| if let ImplItem::Method(x) = items.swap_remove(index)
            { x } else { unreachable!() });


    // get return type before convert converts since it could be quicker
    let return_type = if derive_parsable {
        // get the base parser function to get the return type from
        let base_parser = if defaults.is_some()
            { parser_inner_func.clone().expect("Must have a `parser_inner` function if deriving parsable and defaults are specified.") }
        else
            {       parser_func.clone().expect("Must have a `parser` function to derive parsable") };

        // check if the return value is tagged to not be converted
        let no_convert = base_parser.attrs.iter().find(|Attribute{ path, .. }| path.is_ident("no_convert")).is_some();

        Some(if convert && !no_convert {
            if let ReturnType::Type(_, typ) = base_parser.sig.output { *typ }
            else { panic!("Base parser must have a return type") }
        } else {
            get_return_type_from_parser_func(&base_parser).to_owned()
        })
    } else { None };

    // convert `parser` and `parser_inner` functions
    let ( parser_func, parser_inner_func ) = if convert { (
              parser_func.map(convert_impl_item_method).map(Into::into),
        parser_inner_func.map(convert_impl_item_method).map(Into::into),
    ) } else { (
              parser_func.map(|method| method.to_token_stream()),
        parser_inner_func.map(|method| method.to_token_stream()),
    ) };

    let mut base = quote!{
        impl #parent_type {
            #parser_func
            #parser_inner_func
            #(#items)*
        }
    };

    if derive_parsable {
        base.extend(quote!{
            #[automatically_derived]
            impl Parsable<#return_type> for #parent_type {
                fn parse(string: &str) -> #return_type {
                    let len = string.len();

                    #parent_type::parser().parse(chumsky::Stream::from_iter(len..len+1,
                        crate::lexer::create().parse(
                            string
                        ).expect(concat!("Failed to lex {}", stringify!(#parent_type))).into_iter()
                    )).expect(concat!("Failed to parse {}", stringify!(#parent_type)))
                }
            }

            #[automatically_derived]
            impl From<&str> for #return_type {
                /// Converts a string into a [`#parent_type`]
                ///
                /// ### Panics
                /// 
                /// - If the lexer or parser fails
                fn from(string: &str) -> Self {
                    #parent_type::parse(string)
                }
            }
        })
    }

    if let Some(defaults) = defaults {
        base.extend(quote!{
            #[automatically_derived]
            impl #parent_type {
                pub fn parser() -> impl Parser<Token, #return_type, Error = Simple<Token>> {
                    Self::parser_inner(#defaults)
                }
            }
        })
    }
    base.into()
}

fn convert_impl_item_method(f: ImplItemMethod) -> TokenStream {
    match f {
        ImplItemMethod {
            attrs,
            vis,
            block,
            sig: Signature {
                ident,
                generics,
                inputs,
                output: ReturnType::Type(_, output),
                ..
            },
            ..
        } => {
            // remove no_convert attr from list
            let filtered_attrs: Vec<_> = attrs.iter().filter(|Attribute{ path, .. }| !path.is_ident("no_convert")).collect();

            // if there are more than one input, then add generics
            let ( lifetime, generics ) = if inputs.len() > 1 {
                // only add to the generic param list if there's already parameters
                let generics = if generics.lt_token.is_some() {
                    match generics {
                        Generics { params, where_clause, .. } => 
                            quote! { <#params, 'a> #where_clause },
                    }
                } else { quote! { <'a> } };

                ( quote! { + 'a }, generics )
            } else { ( quote! {}, generics.to_token_stream() ) };

            // closure for converting a type to a parser
            let convert_fn = |typ| quote! { impl Parser<Token, #typ, Error = Simple<Token>> + Clone #lifetime };

            // convert the return value if necessary
            let return_type = if filtered_attrs.len() < attrs.len() {
                quote! { #output }
            } else {
                convert_fn(output)
            };

            // convert inputs
            let inputs: Vec<_> = inputs.into_iter().map(|arg|
                if let FnArg::Typed(PatType{ attrs, pat, ty, .. }) = arg {
                    // #[no_convert] check
                    let new_attrs: Vec<_> = attrs.iter().filter(|attr| !attr.path.is_ident("no_convert")).collect();
                    // convert if needed
                    let ty = if new_attrs.len() < attrs.len() 
                        { ty.to_token_stream() }
                    else
                        { convert_fn(ty) };

                    // remake inputs
                    quote! { #(#new_attrs)* #pat: #ty }
                } else { quote! { #arg } }
            ).collect();

            quote! {
                #(#filtered_attrs)*
                #vis fn #ident #generics(#(#inputs),*) -> #return_type #block
            }.into()
        },
        _ => panic!("Function must have a return type"),
    }
}
