extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::{quote, ToTokens};
use syn::{self, *, punctuated::Punctuated};



/// Convertes an [impl item](syn::ItemImpl) as per the [`parser_util`] macro with specified config
pub fn parser_util(item_ast: syn::ItemImpl, convert: bool, derive_parsable: bool, defaults: Option<Punctuated<Expr, Token![,]>>) -> TokenStream {
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

    // convert other functions if needed
    let items: Vec<_> = items.into_iter()
        .map(|item| 
            if convert { 
                match item {
                    // if it's marked
                    ImplItem::Method(x) => if x.attrs.iter().position(|attr| attr.path.is_ident("parser")).is_some() {
                        // convert it ( removing the attribute is done in the function )
                        convert_impl_item_method(x).into()
                    // otherwise pass through
                    } else { x.to_token_stream() },
                    other => other.to_token_stream(),
                }
            } else { item.to_token_stream() }
        ).collect();


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
        let typ = convert_type(&return_type.unwrap(), false);

        base.extend(quote!{
            #[automatically_derived]
            impl #parent_type {
                pub fn parser() -> #typ {
                    Self::parser_inner(#defaults)
                }
            }
        })
    }
    base.into()
}


/// Returns true if the [`ImplItem`] is a method and its name matches `string`
fn impl_method_matches(impl_item: &ImplItem, string: &str) -> bool {
    if let ImplItem::Method(impl_item) = impl_item {
        impl_item.sig.ident.to_string() == string
    } else { false }
}

/// Gets the return parser output type from an [`ImplItemMethod`]
///
/// `impl Parser<Token, *this*, Error = ..>`
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

/// Converts a normal type to a parser with it as its output, Clone, and an optional lifetime specifier
///
/// `impl Parser<Token, *typ*, Error = Simple<Token>> + Clone`
pub fn convert_type(typ: &Type, lifetime: bool) -> proc_macro2::TokenStream {
    let mut converted = quote!{ impl Parser<Token, #typ, Error = ParserError> + Clone };
    if lifetime { converted.extend(quote!{ + 'a }) }
    converted
}

/// Converts an [`ImplItemMethod`]'s arguments and return values into parsers using [`convert_type`] unless #\[no_convert\] is specified
fn convert_impl_item_method(f: ImplItemMethod) -> TokenStream {
    match f {
        ImplItemMethod {
            attrs,
            vis,
            block,
            sig,
            ..
        } => convert_fn_inner(false, attrs, vis, sig, block),
    }
}

/// Converts a function's arguments and return values into parsers using [`convert_type`] unless #\[no_convert\] is specified
pub fn convert_fn(always_lifetime: bool, f: ItemFn) -> TokenStream {
    match f {
        ItemFn {
            attrs,
            vis,
            block,
            sig,
            ..
        } => convert_fn_inner(always_lifetime, attrs, vis, sig, *block),
    }
}

/// Converts a function's arguments and return values into parsers using [`convert_type`] unless #\[no_convert\] is specified
fn convert_fn_inner(always_lifetime: bool, attrs: Vec<Attribute>, vis: Visibility, sig: Signature, block: Block) -> TokenStream {
    match sig {
        Signature {
            ident,
            generics,
            inputs,
            output: ReturnType::Type(_, output),
            ..
        } => {
            // remove no_convert attr from list
            let filtered_attrs: Vec<_> = attrs.iter().filter(|Attribute{ path, .. }| !path.is_ident("no_convert")).collect();

            // if there are more than one input, then add generics
            let lifetime = always_lifetime || inputs.len() > 1;
            let generics = if lifetime {
                // only add to the generic param list if there's already parameters
                if generics.lt_token.is_some() {
                    match generics {
                        Generics { params, where_clause, .. } => 
                            quote! { <#params, 'a> #where_clause },
                    }
                } else { quote! { <'a> } }
            } else { generics.to_token_stream() };

            // convert the return value if necessary
            let return_type = if filtered_attrs.len() < attrs.len() {
                quote! { #output }.into()
            } else {
                convert_type(&output, lifetime)
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
                        { convert_type(&ty, lifetime) };

                    // remake inputs
                    quote! { #(#new_attrs)* #pat: #ty }
                } else { quote! { #arg } }
            ).collect();

            let filtered_attrs: Vec<_> = attrs.iter().filter(|Attribute{ path, .. }| !path.is_ident("parser")).collect();

            quote! {
                #(#filtered_attrs)*
                #vis fn #ident #generics(#(#inputs),*) -> #return_type #block
            }.into()
        },
        _ => panic!("Function must have a return type"),
    }
}
