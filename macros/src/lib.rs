extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span};
use quote::quote;
use syn::{self, *};

//TODO: token! macro ex: token!(func)

#[proc_macro_attribute]
pub fn parser_util(_: TokenStream, item: TokenStream) -> TokenStream {
    // let attr: syn::Type = syn::parse(attr).unwrap();
    let item_ast: syn::ItemImpl = syn::parse(item.clone()).unwrap();

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

    // println!("Type: {:#?}", return_typ);

    // quote!{}.into()
    let mut out = item.clone();

    out.extend(TokenStream::from(quote! {
        //TODO: extract from into a method named parse and rename from_offset_string to parse_offset
        impl #typ {
            pub fn from_offset_string(offset: usize, string: &str) -> #return_type {
                offset_string(offset, string).as_str().into()
            }
        }

        impl From<&str> for #return_type {
            /// Converts a string into a [`#typ`]
            /// 
            /// ### Panics
            /// 
            /// - If the lexer or parser fails
            fn from(string: &str) -> Self {
                lex_to_parse(string, parse!(#typ), stringify!(#typ))
            }
        }
    }));

    out
    // item
}