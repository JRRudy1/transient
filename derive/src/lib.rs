//! Defines the [`Transient`][crate::Transient] derive macro that implements the
//! [`Transient`][transient::tr::Transient] trait for a struct with at most 1
//! lifetime parameter.
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::fmt;
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::{parse_macro_input, spanned::Spanned, DeriveInput, Ident, Lifetime, TypeParamBound};
use syn::{GenericParam, LifetimeParam, Result as SynResult, Token};

/// Derive macro to implement the `Transient` trait for a struct (or enum).
///
/// # Customization
/// By default, the [variance] is assumed to be _invariant_ with respect to each
/// of it's lifetime parameters. However, there are a variety of situations where
/// you might want a more precise variance. In these situations, you can specify
/// what variance you believe your type has, and this macro will emit additional
/// code that validates the implementation. Right now, this often causes unhelpful
/// error messages, since the compiler assumes you want to change the generated function
/// signature, and we have to way to tell the compiler that the generated code only exists
/// to validate the trait implementation.
///
/// To set a specific variance on your type, annotate the struct with `#[variance(...)]`,
/// providing the lifetime, and variance you want it to have.
///
/// |  Keyword | Alias | Description |
/// | :-  | :- | :- |
/// | `invariant` | `inv` | Declares a _invariant_ relationship with the lifetime; this is the default for types with a lifetime parameter.
/// | `covariant` | `co` | Declares a _covariant_ relationship with the lifetime
/// | `contravariant` | `contra` | Declares a _covariant_ relationship with the lifetime
///
/// # Examples
/// Here the derive assumes the lifetime `'a` is _invariant_.
/// ```rust
/// use transient::Transient;
/// 
/// #[derive(Transient)]
/// struct S<'a> {
///     value: &'a str,
/// }
/// ```
///
/// Here the derive verifies the lifetime `'a` is _covariant_.
/// ```rust
/// use transient::Transient;
/// 
/// #[derive(Transient)]
/// #[variance('a = covariant)] // or #[variance('a = co)]
/// struct S<'a> {
///     value: &'a str,
/// }
/// ```
///
/// # Notes
/// If the type does not have any lifetime parameters, this derive will generate an implementation of
/// `Static`, which has a blanket implementation of `Transient`.
#[proc_macro_derive(Transient, attributes(variance))]
pub fn derive_transient(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let tokens = generate_impl(input).unwrap_or_else(|e| e.into_compile_error());
    TokenStream::from(tokens)
}

fn generate_impl(input: DeriveInput) -> SynResult<TokenStream2> {
    let span = input.span();
    let name = &input.ident;
    let attrs = input
        .attrs
        .iter()
        .map(|a| &a.meta)
        .filter(|a| a.path().is_ident("variance"))
        .map(|a| {
            a.require_list()
                .and_then(|l| l.parse_args_with(parse_variance))
        })
        .collect::<SynResult<Vec<_>>>()?;
    let bounds: Vec<_> = attrs.into_iter().flatten().collect();

    for life in &bounds {
        if !input
            .generics
            .lifetimes()
            .any(|l| l.lifetime == life.lifetime)
        {
            return Err(syn::Error::new(
                life.lifetime.span(),
                format!("Lifetime `{}` is not defined", life.lifetime),
            ));
        }
    }
    let mut generics = input.generics.clone();
    for t in generics.type_params_mut() {
        t.bounds
            .push(TypeParamBound::Lifetime(Lifetime::new("'static", span)));
    }
    let (impl_gen, ty_gen, where_clause) = generics.split_for_impl();

    if input.generics.lifetimes().count() == 0 {
        return Ok(quote! {
            impl #impl_gen ::transient::Static for #name #ty_gen
                #where_clause {}
        });
    }
    
    let mut checks = vec![];
    let transience = input
        .generics
        .lifetimes()
        .map(|l| {
            let var: Vec<_> = bounds
                .iter()
                .filter(|v| v.lifetime == l.lifetime)
                .take(2)
                .collect();
            if var.is_empty() {
                let life = &l.lifetime;
                let var = VarianceTy::Invariant;
                Ok(quote! { #var<#life> })
            } else if var.len() == 1 {
                let life = &l.lifetime;
                let var = var[0].variance;
                generate_check(name, life, var, &input, &mut checks);
                Ok(quote! { #var<#life> })
            } else {
                Err(syn::Error::new(
                    l.lifetime.span(),
                    "Must provide at most one lifetime clause per lifetime",
                ))
            }
        })
        .collect::<SynResult<Punctuated<TokenStream2, Token![,]>>>()?;

    let mut static_generics = input.generics.clone();
    for l in static_generics.lifetimes_mut() {
        l.lifetime = Lifetime::new("'static", l.lifetime.span());
    }
    let (_i, static_ty_gen, _) = static_generics.split_for_impl();

    let checks_name = Ident::new(&format!("__validate_{}", name), name.span());

    Ok(quote! {
        unsafe impl #impl_gen ::transient::Transient for #name #ty_gen
            #where_clause
        {
            type Static = #name #static_ty_gen;
            type Transience = (#transience);
        }
        #[allow(unused)]
        #[allow(non_snake_case)]
        fn #checks_name() {
            #(#checks)*
        }
    })
}

struct VarianceParam {
    lifetime: Lifetime,
    variance: VarianceTy,
}

fn parse_variance(a: ParseStream) -> SynResult<Punctuated<VarianceParam, Token![,]>> {
    a.parse_terminated(
        |a| {
            let lifetime: Lifetime = a.parse()?;
            let _eq: Token![=] = a.parse()?;
            let variance: VarianceTy = a.parse()?;
            Ok(VarianceParam { lifetime, variance })
        },
        Token![,],
    )
}

#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
enum VarianceTy {
    Invariant,
    CoVariant,
    ContraVariant,
    // Static,
}

impl Parse for VarianceTy {
    fn parse(input: ParseStream) -> SynResult<Self> {
        let ident: Ident = input.parse()?;
        if ident == "invariant" || ident == "inv" {
            Ok(Self::Invariant)
        } else if ident == "covariant" || ident == "co" {
            Ok(Self::CoVariant)
        } else if ident == "contravariant" || ident == "contra" {
            Ok(Self::ContraVariant)
        } else {
            Err(syn::Error::new_spanned(
                ident,
                "Variance type must be one of\
                [invariant, inv, covariant, co, contravariant, contra]",
            ))
        }
    }
}

impl ToTokens for VarianceTy {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        match self {
            Self::Invariant => tokens.extend(quote! { ::transient::Inv }),
            Self::CoVariant => tokens.extend(quote! { ::transient::Co }),
            Self::ContraVariant => tokens.extend(quote! { ::transient::Contra }),
        }
    }
}

impl fmt::Display for VarianceTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

fn generate_check(
    name: &Ident,
    life: &Lifetime,
    var: VarianceTy,
    input: &DeriveInput,
    checks: &mut Vec<TokenStream2>,
) {
    match var {
        VarianceTy::Invariant => (), // Always safe
        VarianceTy::CoVariant => {
            let ident = Ident::new(&format!("__validate_{}_{}", name, life.ident), name.span());
            // let ident_ty = Ident::new(&format!("{}_{}", name, life.ident), name.span());
            let mut generics = input.generics.clone();
            // For now, we assume that all generics MUST be static
            for ty in generics.type_params_mut() {
                ty.bounds.push(TypeParamBound::Lifetime(Lifetime::new(
                    "'static",
                    name.span(),
                )));
            }
            let mut impl_gen = generics.clone();
            let test_lifetime = Lifetime::new("'__test_lifetime", name.span());
            let mut param_lifetime = LifetimeParam::new(test_lifetime.clone());
            param_lifetime.bounds.push(life.clone());
            impl_gen
                .params
                .insert(0, GenericParam::Lifetime(param_lifetime));
            let mut param_gen = generics.clone();
            for l in param_gen.lifetimes_mut() {
                if &l.lifetime == life {
                    l.lifetime = test_lifetime.clone();
                }
            }

            let (_, param_gen, _) = param_gen.split_for_impl();
            let (_, generics, _) = generics.split_for_impl();

            checks.push(quote! {
                #[allow(unused)]
                #[allow(non_snake_case)]
                fn #ident #impl_gen (v: #name #param_gen) -> #name #generics {
                    v
                }
            });
        }
        VarianceTy::ContraVariant => {
            let ident = Ident::new(&format!("__validate_{}_{}", name, life.ident), name.span());
            // let ident_ty = Ident::new(&format!("{}_{}", name, life.ident), name.span());
            let mut generics = input.generics.clone();
            // For now, we assume that all generics MUST be static
            for ty in generics.type_params_mut() {
                ty.bounds.push(TypeParamBound::Lifetime(Lifetime::new(
                    "'static",
                    name.span(),
                )));
            }
            let mut impl_gen = generics.clone();
            let test_lifetime = Lifetime::new("'__test_lifetime", name.span());
            let mut param_lifetime = LifetimeParam::new(test_lifetime.clone());
            param_lifetime.bounds.push(life.clone());
            impl_gen
                .params
                .insert(0, GenericParam::Lifetime(param_lifetime));
            let mut param_gen = generics.clone();
            for l in param_gen.lifetimes_mut() {
                if &l.lifetime == life {
                    l.lifetime = test_lifetime.clone();
                }
            }

            let (_, param_gen, _) = param_gen.split_for_impl();
            let (_, generics, _) = generics.split_for_impl();
            checks.push(quote! {
                #[allow(unused)]
                #[allow(non_snake_case)]
                fn #ident #impl_gen (v: #name #generics) -> #name #param_gen {
                    v
                }
            });
        }
    }
}
