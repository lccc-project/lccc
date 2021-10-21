extern crate proc_macro;

use proc_macro::TokenStream;
use syn::{punctuated::Punctuated, Type};

#[proc_macro_attribute]
pub fn xlang_trait(_attrs: TokenStream, ts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(ts as syn::ItemTrait);
    let mut assoc_types = Vec::new();
    let mut methods = Vec::new();
    let generics = input.generics.clone();
    let mut recievers = Vec::<Type>::new();

    let trait_name = input.ident.clone();
    let trait_name_span = trait_name.span();

    let trait_type = {
        let tok = syn::Token![dyn](trait_name_span);
        let mut generic_args = syn::AngleBracketedGenericArguments {
            colon2_token: Some(syn::Token![::](trait_name_span)),
            lt_token: syn::Token![<](trait_name_span),
            args: Punctuated::new(),
            gt_token: syn::Token![>](trait_name_span),
        };

        for g in &generics.params {
            match g {
                syn::GenericParam::Type(t) => {
                    let arg = syn::GenericArgument::Type(Type::Path(syn::TypePath {
                        qself: None,
                        path: syn::Path {
                            leading_colon: None,
                            segments: {
                                let mut seg = Punctuated::new();
                                seg.push(syn::PathSegment {
                                    ident: t.ident.clone(),
                                    arguments: syn::PathArguments::None,
                                });

                                seg
                            },
                        },
                    }));
                    generic_args.args.push(arg);
                }
                syn::GenericParam::Lifetime(lt) => unimplemented!(),
                syn::GenericParam::Const(c) => unimplemented!(),
            }
        }
    };

    for item in &input.items {
        match item {
            syn::TraitItem::Const(_) => {
                panic!("Associated Constants Aren't allowed for abi_safe traits")
            }
            syn::TraitItem::Method(m) => methods.push(m.clone()),
            syn::TraitItem::Type(t) => assoc_types.push(t.clone()),
            syn::TraitItem::Macro(_) => {
                panic!("Cannot handle internal macros in trait definitions")
            }
            syn::TraitItem::Verbatim(_) => panic!(),
            _ => unreachable!(),
        }
    }

    for m in &methods {
        if let Some(arg) = m.sig.inputs.first() {
            match arg {
                syn::FnArg::Receiver(r) => {}
                syn::FnArg::Typed(_) => unimplemented!(),
            }
        }
    }

    Into::into(quote::quote! {
        #input

        const _: () = {
            fn __check_reciever<__T: ::xlang_abi::traits::AbiSafeTrait + ?Sized,__R: ::xlang_abi::traits::AbiSafeReciever<__T>>(){}
            fn __validate #generics (){

            }
        };
    })
}
