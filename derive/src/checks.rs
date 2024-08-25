//! Defines functionality for generating the validation module ensuring that the
//! variances declared using the macro attributes are valid for the type.
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_quote, parse_quote_spanned, spanned::Spanned};
use syn::{GenericParam, Generics, Ident, LifetimeParam, WherePredicate};

use super::{SelfType, TransientImpl, TypeWithGenerics, Variance, VarianceKind};

/// module generated to force a compile error if an invalid variance is selected
pub(super) struct ChecksModule<'a> {
    name: Ident,
    funcs: Vec<CheckFn<'a>>,
}

impl<'a> ChecksModule<'a> {
    #[rustfmt::skip]
    pub(super) fn new(impl_: &'a TransientImpl<'a>) -> Option<Self> {
        let TransientImpl(self_type, _, transience, _) = impl_;
        let name = Ident::new(
            &format!("__validate_{}", self_type.name),
            self_type.name.span(),
        );
        let funcs = transience.0.iter()
            .filter_map(|variance| CheckFn::new(self_type, variance))
            .collect::<Vec<_>>();
        if funcs.is_empty() {
            None
        } else {
            Some(ChecksModule { name, funcs })
        }
    }
}

impl<'a> ToTokens for ChecksModule<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let ChecksModule { name, funcs } = self;
        tokens.extend(quote!(
            mod #name {
                #![allow(non_snake_case, dead_code)]
                use super::*;
                #(#funcs)*
            }
        ));
    }
}

struct CheckFn<'a> {
    func_name: Ident,
    // name and generics for the function's argument
    arg_type: TypeWithGenerics<'a>,
    // same as the implementing type
    return_type: &'a SelfType,
    // generics and where clause for the function
    generics: Generics,
}
impl<'a> CheckFn<'a> {
    #[allow(clippy::needless_late_init)]
    fn new(target_type: &'a SelfType, variance: &'a Variance) -> Option<Self> {
        let lt = &variance.lifetime;
        let func_name: String;
        let test_lt: LifetimeParam;
        let where_pred: WherePredicate;
        match variance.kind {
            VarianceKind::Inv(_) => return None,
            VarianceKind::Co(span) => {
                func_name = format!("covariant_wrt_{}", lt.ident);
                test_lt = parse_quote_spanned!(span => '__long);
                where_pred = parse_quote!(#test_lt: #lt);
            }
            VarianceKind::Contra(span) => {
                func_name = format!("contravariant_wrt_{}", lt.ident);
                test_lt = parse_quote_spanned!(span => '__short);
                where_pred = parse_quote!(#lt: #test_lt);
            }
        };
        // generics for the func; original with the test lifetime inserted
        let mut func_generics = target_type.generics.clone();
        func_generics
            .params
            .insert(0, GenericParam::Lifetime(test_lt.clone()));
        func_generics
            .make_where_clause()
            .predicates
            .push(where_pred);
        // generics for the funcs argument; original with 'a replaced by 'test
        let mut arg_generics = target_type.generics.clone();
        arg_generics
            .lifetimes_mut()
            .find(|param| param.lifetime.eq(lt))
            .map(|param| *param = test_lt)?;
        Some(CheckFn {
            func_name: Ident::new(&func_name, variance.span()),
            generics: func_generics,
            return_type: target_type,
            arg_type: TypeWithGenerics {
                name: &target_type.name,
                generics: arg_generics,
            },
        })
    }
}

impl<'a> ToTokens for CheckFn<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let CheckFn {
            func_name,
            arg_type,
            return_type,
            generics,
        } = self;
        let (func_generics, _, where_clause) = generics.split_for_impl();
        tokens.extend(quote!(
            fn #func_name #func_generics(v: #arg_type) -> #return_type
            #where_clause
            {
                v
            }
        ));
    }
}
