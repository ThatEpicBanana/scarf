extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{self, *};

//TODO: token! macro ex: token!(func)

/// Assigned on a `Parsable` impl block with a `parser` function. <br>
/// The output doesn't have to be the same as the type that will implement `Parsable.`
/// 
/// Implies `Parsable` and [`From<&str>`] with a given `parser` function that it then moves to a normal `impl` block. <br>
/// It is recommended to use [`imply`] to imply the parser's existence in the new normal `impl` block to language servers
/// 
/// ### Example:
/// 
/// 
#[proc_macro_attribute]
pub fn derive_parsable_from_impl(_: TokenStream, item: TokenStream) -> TokenStream {
    let item_ast: syn::ItemImpl = syn::parse(item.clone()).unwrap();

    // get parent type from impl
    let typ = &*item_ast.self_ty;
    

    // get trait type ex: Parsable<S<Type>>
    let trait_typ = item_ast.trait_.expect("Impl block must imply Parsable<> for a type (implementation not found)").1;
    
    // get last segment 
    let last = trait_typ.segments.iter().last().unwrap();

    // get output type from trait type
    let return_type = if last.ident == Ident::new("Parsable", Span::call_site()) {
        if let PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) = &last.arguments {
            if let GenericArgument::Type(typ) = &args[0] {
                typ
            } else { panic!("Parsable implied, but first generic argument is not a type"); }
        } else { panic!("Parsable implied, but without angle bracketed generic arguments"); }
    } else { panic!("Impl block must imply Parsable<> for a type (implementation found, but not of the correct identifier)"); };


    // get first function in the impl block with the identifier of `parser`
    let parser_func = item_ast.items.iter()
        .find(|val| match val {
            &ImplItem::Method(ImplItemMethod {
                sig: Signature {
                    ident, ..
                }, ..
            }) => ident == &Ident::new("parser", Span::call_site()),
            _ => false,
        }).expect("No parser function found");
    // you could check if the types are the same, but it's way too complicated as shown by derive_parser_from_impl
    
    
    quote! {
        impl #typ {
            #parser_func
        }

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
    }.into()
}




/// Implies `Parsable` and [`From<&str>`] using a given `parser` function. <br>
/// It is recommended to use [`imply`] to imply the trait implementation's existence.
/// 
/// **Note:** The output doesn't have to be the same as the type that will implement `Parsable.`
#[proc_macro_attribute]
pub fn derive_parsable(_: TokenStream, item: TokenStream) -> TokenStream {
    let item_ast: syn::ItemImpl = syn::parse(item.clone()).unwrap();

    // get parent type from impl
    let typ = &*item_ast.self_ty;

    // get first function in the impl block with the identifier of `parser`
    let parser_func = item_ast.items.iter()
        .find(|val| match val {
            &ImplItem::Method(ImplItemMethod {
                sig: Signature {
                    ident, ..
                }, ..
            }) => ident == &Ident::new("parser", Span::call_site()),
            _ => false,
        }).expect("No parser function found");

    // slowly go down the tree until we get to the type
    let return_type = match parser_func {
        // return type
        ImplItem::Method(ImplItemMethod {
            sig: Signature {
                output: ReturnType::Type(_, boxed), ..
            }, ..
        }) => 
            // return type with bounds
            match &**boxed {
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
    };


    let mut out = item.clone();

    out.extend(TokenStream::from(quote! {

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

/// Used to imply the existence of an item to a language server.
/// 
/// It really just replaces the item with nothing.
#[proc_macro_attribute]
pub fn imply(_: TokenStream, _: TokenStream) -> TokenStream {
    quote!{}.into()
}