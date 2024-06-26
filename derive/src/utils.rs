//! Miscellaneous internal types and functions
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use std::collections::BTreeSet;
use syn::punctuated::Iter;
use syn::{GenericParam, Generics, Ident, Type, TypeParam, TypeParamBound};

/// Convenience struct grouping the name of a type and its generics
pub(super) struct TypeWithGenerics<'a> {
    pub(super) name: &'a Ident,
    pub(super) generics: Generics,
}

impl<'a> ToTokens for TypeWithGenerics<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let TypeWithGenerics { name, generics } = self;
        let type_generics = generics.split_for_impl().1;
        quote!(#name #type_generics).to_tokens(tokens);
    }
}

/// Extract the non-'static lifetimes parameters from the given generics
pub(super) fn extract_lifetimes(generics: &Generics) -> Vec<syn::Lifetime> {
    generics
        .lifetimes()
        .filter(|lt| lt.lifetime.ident != "static")
        .map(|lt| lt.lifetime.clone())
        .collect::<Vec<_>>()
}

/// Adds a `'static` bound to the `where` clause of the given generics for each 
/// type parameter, skipping those that already have a 'static bound
#[rustfmt::skip]
pub(super) fn insert_static_predicates(generics: &mut Generics) {
    // Add where clause if there isn't one and get a mutable reference to it. We 
    // can't just use the reference returned from the `make_where_clause` call 
    // since that would hold a mutable borrow on the entire `Generics` struct.
    generics.make_where_clause();
    let where_clause = generics.where_clause.as_mut().unwrap();
    // Collect generics type params that do not have a 'static bound
    let mut needs_bound = BTreeSet::<&Ident>::new();
    generics.params.iter()
        .filter_map(param_as_type)
        .filter(|param| !contains_static_bound(param.bounds.iter()))
        .for_each(|param| { needs_bound.insert(&param.ident); });
    // For each type param in the where clause, add a 'static bound if it does 
    // not already have one in either location
    where_clause.predicates.iter_mut()
        .filter_map(predicate_as_type)
        .map(|pred| (&pred.bounded_ty, &mut pred.bounds))
        .filter_map(|(ty, bounds)| Some((type_as_ident(ty)?, bounds)))
        .for_each(|(ident, bounds)| {
            if !needs_bound.remove(ident) { return; }
            if contains_static_bound(bounds.iter()) { return; }
            bounds.push(syn::parse_quote!('static));
        });
    // Add bound for any remaining params that were missed in the last pass since 
    // they did not have an existing where clause predicate
    for ident in &needs_bound {
        where_clause.predicates.push(syn::parse_quote!(#ident: 'static));
    }
}

fn contains_static_bound(mut bounds: Iter<TypeParamBound>) -> bool {
    bounds.any(|bnd| matches!(bnd, TypeParamBound::Lifetime(lt) if lt.ident == "static"))
}

fn param_as_type(param: &GenericParam) -> Option<&TypeParam> {
    if let GenericParam::Type(tp) = param {
        Some(tp)
    } else {
        None
    }
}

fn predicate_as_type(pred: &mut syn::WherePredicate) -> Option<&mut syn::PredicateType> {
    if let syn::WherePredicate::Type(tp) = pred {
        Some(tp)
    } else {
        None
    }
}

fn type_as_ident(ty: &Type) -> Option<&Ident> {
    if let Type::Path(path) = ty {
        path.path.get_ident()
    } else {
        None
    }
}
