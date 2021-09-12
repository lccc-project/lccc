extern crate proc_macro;

use proc_macro::{Span, TokenStream};
use syn::{Path, TraitBound, TypeParamBound, punctuated::Punctuated};

#[proc_macro_attribute]
pub fn xlang_trait(_attrs: TokenStream, ts: TokenStream) -> TokenStream {
    let input = syn::parse_macro_input!(ts as syn::ItemTrait);
    let mut assoc_types = Vec::new();
    let mut methods = Vec::new();
    let generics = input.generics.clone();
    let mut dyn_name = syn::TypeTraitObject{
        dyn_token: Some(syn::Token![dyn](proc_macro2::Span::call_site())),
        bounds: {
            let mut bounds = Punctuated::new();
            bounds.push(TypeParamBound::Trait(TraitBound{ path: Path{ leading_colon: None, segments: {
                let mut seg = Punctuated::new();
                seg.push(PathSegment::PathSegment { ident: (), arguments: None });
                seg
            } }}));
            bounds
        }
    };
    let mut recievers = 

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
    Into::into(quote::quote! {
        #input

        const _: () = {
            fn __check_reciever<T: ::xlang_abi::traits::AbiSafeTrait + ?Sized,R: ::xlang_abi::traits::AbiSafeReciever<T>>(){}
        };
    })
}
