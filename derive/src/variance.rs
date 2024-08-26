//! Types related to variance declarations
use proc_macro2::{Span, TokenStream};
use quote::{quote_spanned, ToTokens};
use std::{collections::BTreeMap, fmt, mem};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{Attribute, Ident, Lifetime, Meta, Token};

use super::{Error, Result};

/// Represents the set of variances declared in the macro attributes.
pub(crate) enum VarianceDeclarations {
    // initial state with no variances declared
    Empty,
    // state after a global declaration
    Global(VarianceKind),
    // state after 1+ specific declarations
    Mapping(BTreeMap<Ident, VarianceKind>),
}

impl VarianceDeclarations {
    pub(crate) fn ensure_empty(&mut self) -> Result<()> {
        match self {
            Self::Mapping(map) if !map.is_empty() => {
                let lifetime = map.keys().next().unwrap().clone();
                Err(Error::UnexpectedLifetime(lifetime))
            }
            _ => Ok(()),
        }
    }

    pub(crate) fn pop_variance<'a>(
        &mut self,
        lifetime: &'a Lifetime,
        options: &'a crate::TransientOptions,
    ) -> Variance<'a> {
        let kind = match self {
            Self::Empty => VarianceKind::default(),
            Self::Global(kind) => kind.clone(),
            Self::Mapping(map) => map.remove(&lifetime.ident).unwrap_or_default(),
        };
        Variance { lifetime, kind, krate: &options.krate }
    }

    pub(crate) fn extract(attrs: &[Attribute]) -> Result<Self> {
        let mut decls = Self::Empty;
        for attr in attrs {
            decls.parse_attr_into(attr)?;
        }
        Ok(decls)
    }

    fn parse_attr_into(&mut self, attr: &Attribute) -> Result<()> {
        let path = attr.path();
        let kind = if path.is_ident("covariant") {
            VarianceKind::Co(path.span())
        } else if path.is_ident("contravariant") {
            VarianceKind::Contra(path.span())
        } else {
            return Ok(());
        };
        // this attr declares a variance, so a global variance must not be set
        if matches!(self, Self::Global(_)) {
            return match mem::replace(self, Self::Empty) {
                Self::Global(old) => Err(Error::DuplicateVariance(old, kind)),
                _ => unreachable!(),
            };
        }
        // this attr declares a global variance, so there must be no existing decls
        if let Meta::Path(_) = attr.meta {
            let old = match mem::replace(self, Self::Empty) {
                Self::Global(old) => old,
                Self::Mapping(map) => map.into_values().last().unwrap(),
                Self::Empty => {
                    *self = Self::Global(kind);
                    return Ok(());
                }
            };
            return Err(Error::DuplicateVariance(old, kind));
        }
        attr.meta
            .require_list()?
            .parse_args_with(Punctuated::<Ident, Token![,]>::parse_terminated)?
            .into_iter()
            .try_for_each(|ident| self.push(ident, kind.clone()))
    }

    fn get_mapping_mut(&mut self) -> Option<&mut BTreeMap<Ident, VarianceKind>> {
        match self {
            Self::Global(_) => None,
            Self::Mapping(mapping) => Some(mapping),
            Self::Empty => {
                *self = Self::Mapping(BTreeMap::new());
                self.get_mapping_mut()
            }
        }
    }

    fn push(&mut self, ident: Ident, kind: VarianceKind) -> Result<()> {
        let map = self.get_mapping_mut().unwrap();
        if let Some(old) = map.insert(ident, kind) {
            let new = map.values().last().unwrap().clone();
            Err(Error::DuplicateVariance(old, new))
        } else {
            Ok(())
        }
    }
}

#[derive(Clone, Debug)]
pub(crate) enum VarianceKind {
    Inv(Span),
    Co(Span),
    Contra(Span),
}

impl VarianceKind {
    pub(crate) fn span(&self) -> Span {
        let (Self::Inv(span) | Self::Co(span) | Self::Contra(span)) = self;
        *span
    }
}

impl Default for VarianceKind {
    fn default() -> Self {
        Self::Inv(Span::call_site())
    }
}

impl fmt::Display for VarianceKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Inv(_) => f.write_str("invariant"),
            Self::Co(_) => f.write_str("covariant"),
            Self::Contra(_) => f.write_str("contravariant"),
        }
    }
}

pub(crate) struct Variance<'a> {
    pub(crate) lifetime: &'a Lifetime,
    pub(crate) kind: VarianceKind,
    pub(crate) krate: &'a syn::Path,
}

impl<'a> ToTokens for Variance<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        use VarianceKind::*;
        let Variance { lifetime: lt, kind, krate } = self;
        tokens.extend(match *kind {
            Inv(span) => quote_spanned!(span => #krate::Inv<#lt>),
            Co(span) => quote_spanned!(span => #krate::Co<#lt>),
            Contra(span) => quote_spanned!(span => #krate::Contra<#lt>),
        });
    }
}
