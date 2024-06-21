//! Defines the [`Transient`][crate::Transient] derive macro that implements the
//! [`Transient`][transient::tr::Transient] trait for a struct with at most 1
//! lifetime parameter.
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use std::fmt;
use syn::parse::{Parse, ParseStream};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote};
use syn::{Data, DataStruct, DeriveInput, Fields, Generics, Ident, Lifetime};

/// Derive macro that implements the  [`Transient`] trait for a struct with at
/// most 1 lifetime parameter.
///
/// When this macro is applied to a `'static` struct with no lifetime parameters,
/// it will instead implement the [`Static`] trait which results in a blanket
/// impl of the `Transient` trait.
///
/// This macro is limited to structs satisfying the following conditions:
/// - There must be at most 1 lifetime parameter. Structs with extra lifetime
///   parameters can easily implement the trait by hand, but care must be taken
///   to ensure that the invariants detailed in the trait's [safety docs] are
///   upheld.
/// - There may be any number of type (or const) parameters, but the trait
///   will only be implemented where `T: 'static` for each type parameter `T`.
///
/// # Customization
/// By default, the [variance] of a deriving struct is assumed to be _invariant_
/// with respect to its lifetime parameter (if it has one), since this is the
/// only type of variance that can be safely used for _all_ types without
/// analyzing the behavior of its fields (which this macro does not attempt to
/// do). When the added flexibility of _covariance_ or _contravariance_ is
/// needed, the "variance(...)" helper attribute can be used to `unsafe`-ly
/// override this default if you are confident that the chosen variance is
/// appropriate for the type; however, you should first review the [safety docs]
/// for the `Transient` trait (particularly related to its `Transience` associated
/// type to ensure that its invariants are upheld.
///
/// To set the variance for your type, annotate one of its fields (preferably
/// either the _first_ field or the field with the lifetime, but any will do)
/// with the `#[variance(...)]` attribute, substituting the ellipsis for one
/// of the following keywords:
///
/// |  Keyword | Alias | Description |
/// | :-  | :- | :- |
/// | `invariant` | `inv` | Declares a _invariant_ relationship with the lifetime; this is the default for types with a lifetime parameter.
/// | `unsafe_covariant` | `unsafe_co` | Declares a _covariant_ relationship with the lifetime; this is `unsafe`.
/// | `unsafe_contravariant` | `unsafe_contra` | Declares a _covariant_ relationship with the lifetime; this is `unsafe`.
///
/// This can fail for any of the following reasons:
/// - Requesting any variance for a type with no lifetime parameters
/// - Requesting co- or contra-variance without the 'unsafe_' prefix
/// - Providing more than one "variance" attribute with conflicting values
///
///
/// # Example - struct with a type parameter and a lifetime parameter
///
/// Invocation:
/// ```
/// use transient::Transient;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Transient)]
/// struct S<'a, T> {
///     value: &'a T
/// }
/// ```
///
/// Generated impl:
/// ```
/// # struct S<'a, T> {value: &'a T}
/// unsafe impl<'a, T: 'static> ::transient::Transient for S<'a, T> {
///     type Static = S<'static, T>;
///     type Transience = ::transient::Inv<'a>;
/// }
/// ```
///
/// # Example - struct with a lifetime parameter and a _covariance_ declaration
///
/// Invocation:
/// ```
/// use transient::Transient;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Transient)]
/// struct S<'a> {
///     #[variance(unsafe_co)]
///     name: String,
///     values: &'a [i32]
/// }
/// ```
///
/// Generated impl:
/// ```
/// # struct S<'a> {name: String, values: &'a [i32]}
/// unsafe impl<'a> ::transient::Transient for S<'a> {
///     type Static = S<'static>;
///     type Transience = ::transient::Co<'a>;
/// }
/// ```
///
/// # Example - struct with a type parameter and `where` clause but no lifetimes
///
/// Invocation:
/// ```
/// use transient::Transient;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Transient)]
/// struct S<T> where T: Clone {
///     value: T
/// }
/// ```
///
/// Generated impl:
/// ```
/// # struct S<T> where T: Clone {value: T}
/// impl<T: 'static> ::transient::Static for S<T> where T: Clone {}
/// ```
///
/// [`Transient`]: ../transient/trait.Transient.html
/// [`Static`]: ../transient/trait.Static.html
/// [safety docs]: ../transient/trait.Transient.html#Safety
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
#[proc_macro_derive(Transient, attributes(variance))]
pub fn derive_transient(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    match Params::extract(input) {
        Ok(Params::Transient(params)) => impl_transient(params).into(),
        Ok(Params::Static(params)) => impl_static(params).into(),
        Err(err) => err.into_compile_error().into(),
    }
}

/// generate `Static` impl for the 'static type
fn impl_static(params: StaticParams) -> TokenStream2 {
    let trait_ = quote!(::transient::Static);
    let name = params.name;
    let (impl_generics, ty_generics, where_) = params.generics.split_for_impl();
    quote!(
        impl #impl_generics #trait_ for #name #ty_generics #where_ {}
    )
}

/// generate `Transient` impl for the non-'static type
fn impl_transient(params: TransientParams) -> TokenStream2 {
    let trait_ = quote!(::transient::Transient);
    let (name, variance) = (params.name, params.variance);
    let (impl_generics, ty_generics, where_) = params.generics.split_for_impl();
    let (_, static_generics, _) = params.static_generics.split_for_impl();
    quote!(
        unsafe impl #impl_generics #trait_ for #name #ty_generics #where_ {
            type Static = #name #static_generics;
            type Transience = #variance;
        }
    )
}

/// AST struct containing the relevant pieces from the `DeriveInput`
struct Input {
    name: Ident,
    generics: Generics,
    fields: Fields,
}

impl Parse for Input {
    #[rustfmt::skip]
    fn parse(stream: ParseStream) -> syn::Result<Self> {
        let input = stream.parse::<DeriveInput>()?;
        let span = input.span();
        let DeriveInput { ident, generics, data, .. } = input;
        if let Data::Struct(DataStruct { fields, .. }) = data {
            Ok(Input { name: ident, generics, fields })
        } else {
            Err(Error::NotAStruct(span).into())
        }
    }
}

/// processed info for implementing either the `Static` or `Transient` trait
enum Params {
    Static(StaticParams),
    Transient(TransientParams),
}

impl Params {
    fn extract(mut input: Input) -> Result<Self> {
        Self::add_static_bounds(&mut input.generics);
        match Self::get_lifetime(&input.generics)? {
            None => StaticParams::extract(input).map(Params::Static),
            Some(lt) => TransientParams::extract(input, lt).map(Params::Transient),
        }
    }

    fn add_static_bounds(generics: &mut Generics) {
        generics
            .type_params_mut()
            .for_each(|param| param.bounds.push(parse_quote!('static)))
    }

    fn get_lifetime(generics: &Generics) -> Result<Option<Lifetime>> {
        let mut lifetimes = generics.lifetimes();
        let lifetime = lifetimes.next().map(|lt| lt.lifetime.clone());
        match lifetimes.next() {
            Some(lifetime2) => Err(Error::TooManyLifetimes(lifetime2.span())),
            None => Ok(lifetime),
        }
    }
}

/// processed info for implementing the `Static` trait
struct StaticParams {
    name: Ident,
    generics: Generics,
}

impl StaticParams {
    /// extract info from the input AST
    fn extract(input: Input) -> Result<Self> {
        Self::reject_variance(&input.fields)?;
        Ok(StaticParams {
            name: input.name,
            generics: input.generics,
        })
    }

    /// ensure that no fields have a "variance" attribute
    fn reject_variance(fields: &Fields) -> Result<()> {
        for field in fields.iter() {
            for attr in field.attrs.iter() {
                if attr.path().is_ident("variance") {
                    return Err(Error::StaticTypeWithVariance(attr.span()));
                }
            }
        }
        Ok(())
    }
}

/// processed info for implementing the `Transient` trait
struct TransientParams {
    name: Ident,
    variance: Variance,
    generics: Generics,
    static_generics: Generics,
}

impl TransientParams {
    /// extract info from the input AST
    fn extract(input: Input, lifetime: Lifetime) -> Result<Self> {
        Ok(TransientParams {
            name: input.name,
            variance: Self::get_variance(&input.fields, lifetime)?,
            static_generics: Self::get_static_generics(&input.generics),
            generics: input.generics,
        })
    }

    /// search field attributes for a variance declaration or use the default
    fn get_variance(fields: &Fields, lifetime: Lifetime) -> Result<Variance> {
        let mut kind: Option<VarianceKind> = None;
        for field in fields.iter() {
            for attr in field.attrs.iter() {
                if attr.path().is_ident("variance") {
                    if let Some(old) = kind.replace(attr.parse_args()?) {
                        let new = kind.take().unwrap();
                        return Err(Error::DuplicateVariance { old, new });
                    }
                }
            }
        }
        let kind = kind.unwrap_or_else(|| VarianceKind::Inv(Span::call_site()));
        Ok(Variance { kind, lifetime })
    }

    /// get a copy of the generics with lifetimes replaced by 'static
    fn get_static_generics(generics: &Generics) -> Generics {
        let mut static_generics = generics.clone();
        static_generics.lifetimes_mut().for_each(|lt| {
            *lt = parse_quote!('static);
        });
        static_generics
    }
}

#[derive(Clone, Debug)]
enum VarianceKind {
    Inv(Span),
    Co(Span),
    Contra(Span),
}

#[allow(clippy::type_complexity)]
impl VarianceKind {
    /// mapping of valid keys to constructors
    const MAP: [(&'static str, fn(Span) -> VarianceKind); 6] = [
        ("inv", VarianceKind::Inv),
        ("invariant", VarianceKind::Inv),
        ("unsafe_co", VarianceKind::Co),
        ("unsafe_covariant", VarianceKind::Co),
        ("unsafe_contra", VarianceKind::Contra),
        ("unsafe_contravariant", VarianceKind::Contra),
    ];

    /// keys that are recognized but forbidden
    const UNSAFE_KEYS: [&'static str; 4] = ["co", "covariant", "contra", "contravariant"];

    fn span(&self) -> Span {
        match self {
            Self::Inv(span) => *span,
            Self::Co(span) => *span,
            Self::Contra(span) => *span,
        }
    }
}

impl Parse for VarianceKind {
    /// attempt to parse a variance specifier from the stream
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let span = input.span();
        let mut string = input.parse::<Ident>()?.to_string();
        string.make_ascii_lowercase();
        if let Some((_, f)) = VarianceKind::MAP.iter().find(|(v, _)| v.eq(&string)) {
            Ok(f(span))
        } else if VarianceKind::UNSAFE_KEYS.iter().any(|v| v.eq(&string)) {
            Err(Error::UnsafeVariance { string, span }.into())
        } else {
            Err(Error::UnexpectedVariance { string, span }.into())
        }
    }
}

impl fmt::Display for VarianceKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VarianceKind::Inv(_) => f.write_str("invariant"),
            VarianceKind::Co(_) => f.write_str("covariant"),
            VarianceKind::Contra(_) => f.write_str("contravariant"),
        }
    }
}

#[derive(Debug)]
struct Variance {
    kind: VarianceKind,
    lifetime: Lifetime,
}

impl ToTokens for Variance {
    fn to_tokens(&self, stream: &mut TokenStream2) {
        let lt = &self.lifetime;
        let tokens = match self.kind {
            VarianceKind::Inv(_) => quote!(::transient::Inv<#lt>),
            VarianceKind::Co(_) => quote!(::transient::Co<#lt>),
            VarianceKind::Contra(_) => quote!(::transient::Contra<#lt>),
        };
        stream.extend(tokens);
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Syn(#[from] syn::Error),
    #[error("Only `struct`'s are supported!")]
    NotAStruct(Span),
    #[error("At most one lifetime parameter is allowed!")]
    TooManyLifetimes(Span),
    #[error("A variance cannot be requested for a struct without \na lifetime parameter!\n ")]
    StaticTypeWithVariance(Span),
    #[error("Duplicate variance specification! '{old}' replaced by '{new}'\n ")]
    DuplicateVariance {
        old: VarianceKind,
        new: VarianceKind,
    },
    #[error(
        "Unexpected variance argument '{string}'! The valid options are: \n{:?}\n ",
        VarianceKind::MAP.iter().map(|(k, _)| *k).collect::<Vec<_>>()
    )]
    UnexpectedVariance { string: String, span: Span },
    #[error(
        "Setting the variance to '{string}' is `unsafe`! Prefix the argument with \n\
        'unsafe_' ('#[variance(unsafe_{string})]') after reviewing the safety docs \n\
        for the `transient::Transient` trait.\n "
    )]
    UnsafeVariance { string: String, span: Span },
}

impl Error {
    fn into_compile_error(self) -> TokenStream2 {
        syn::Error::from(self).into_compile_error()
    }
    fn span(&self) -> Span {
        match self {
            Error::Syn(err) => err.span(),
            Error::NotAStruct(span) => *span,
            Error::TooManyLifetimes(span) => *span,
            Error::StaticTypeWithVariance(span) => *span,
            Error::DuplicateVariance { new, .. } => new.span(),
            Error::UnexpectedVariance { span, .. } => *span,
            Error::UnsafeVariance { span, .. } => *span,
        }
    }
}

impl From<Error> for syn::Error {
    fn from(value: Error) -> Self {
        syn::Error::new(value.span(), value.to_string())
    }
}
