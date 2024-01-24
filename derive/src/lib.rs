//!
//! Invocation:
//! ```
//! use transient_any_derive::MakeStatic;
//!
//! #[derive(Debug, Clone, PartialEq, Eq, MakeStatic)]
//! struct S<'a, T: 'static> {
//!     value: &'a T,
//! }
//! ```
//! Generated impl:
//! ```
//! # pub mod transient_any {pub unsafe trait MakeStatic<'a> {type Static;}}
//! # struct S<'a, T> {value: &'a T}
//!
//! unsafe impl<'a, T: 'static> transient_any::MakeStatic<'a> for S<'a, T> {
//!     type Static = S<'static, T>;
//! }
//! ```
//!
//! unsafe impl #impl_generics transient_any::MakeStatic <#lt> for #ident #ty_generics
//! #where_clause
//! {
//!     type Static = #ident<#static_ty_generics>;
//! }

#![allow(unused, dead_code)]
use std::compile_error;
use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenTree, TokenStream as TokenStream2};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    punctuated::Punctuated,    
    parse::{Nothing, ParseStream, Parser},
    parenthesized, parse_macro_input, token, Abi, Attribute,
    Data, DeriveInput, Error, Expr, Field, Generics, Path,
    Result, Token, Type, Visibility, Fields, GenericParam,
    ImplGenerics, LifetimeParam, TypeGenerics, parse_quote,
    TypeParamBound, Lifetime,
};
use syn::spanned::Spanned;


fn static_type_bound() -> TypeParamBound {
    parse_quote! { 'static }
}

fn static_param() -> GenericParam {
    parse_quote! { 'static }
}

fn trait_path() -> Path {
    parse_quote! { transient_any::MakeStatic }
}

// fn trait_path_and_lifetime(lifetime: &Lifetime) -> TokenStream2 {
//     quote! { ::transient_any::MakeStatic<#lifetime> }
// }


fn generate_impl(input: DeriveInput) -> Result<TokenStream2> {

    // original generics and where clause for the struct
    let (_, ty_generics, where_clause) = input.generics.split_for_impl();

    // generics for the `Static` type (same as orig, but `'a` replaced by `'static`)
    let mut static_generics = vec![static_param()];

    // generics for impl<....> (same as orig, but with `'static` added to any type params)
    let mut impl_generics = input.generics.clone();
    let mut params_iter = impl_generics.params.iter_mut();

    // get lifetime from the first parameter
    let lifetime = match params_iter.next() {
        Some(GenericParam::Lifetime(lt)) => lt.lifetime.clone().to_token_stream(),
        _ => return Err(Error::new(
            input.generics.span(),
            "Exactly one lifetime parameter is required!"
        ))
    };

    // process remaining params
    for param in params_iter {
        if let GenericParam::Lifetime(_) = param {
            return Err(Error::new(
                input.generics.span(),
                "Exactly one lifetime parameter is required!"
            ))
        }
        static_generics.push(param.clone());
        if let GenericParam::Type(ty) = param {
            ty.bounds.push(static_type_bound());
        }
    }

    // get the `Static` type generics
    let _generics: Generics = parse_quote!(<#(#static_generics,)*>);
    let static_ty_generics = _generics.split_for_impl().1;

    // get name of the input struct and qualified path to the trait
    let struct_ = input.ident;
    let trait_ = trait_path();

    // generate impl block
    let tokens = quote!(
        unsafe impl #impl_generics #trait_ <#lifetime> for #struct_ #ty_generics
        #where_clause
        {
            type Static = #struct_ #static_ty_generics;
        }
    );

    Ok(tokens)
}


/// Derive the `MakeStatic` trait.
#[proc_macro_derive(MakeStatic)]
pub fn derive_make_static(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);

    let tokens = generate_impl(input)
        .unwrap_or_else(|e| {
            let err = e.to_compile_error();
            quote!(
                fn _fail() {
                    #err
                }
            )
        });

    TokenStream::from(tokens)
}
