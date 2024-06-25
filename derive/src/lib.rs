//! Defines the [`Transient`][crate::Transient] derive macro that implements the
//! [`Transient`][transient::tr::Transient] trait for a struct with at most 1
//! lifetime parameter.
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned, ToTokens};
use std::borrow::Cow;
use std::collections::BTreeMap;
use std::fmt;
use std::ops::Deref;
use syn::parse::{Parse, ParseStream};
use syn::punctuated;
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote};
use syn::{Attribute, ItemImpl, Meta, Token, Type};
use syn::{Data, DataStruct, DeriveInput, Fields, Generics, Ident, Lifetime};
use syn::{GenericParam, LifetimeParam, TypeGenerics, WherePredicate};
use syn::{ImplGenerics, WhereClause};

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
///     type Transience = transient::Inv<'a>;
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
///     type Transience = transient::Co<'a>;
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
/// impl<T: 'static> transient::Static for S<T> where T: Clone {}
/// ```
///
/// [`Transient`]: ../transient/trait.Transient.html
/// [`Static`]: ../transient/trait.Static.html
/// [safety docs]: ../transient/trait.Transient.html#Safety
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
#[proc_macro_derive(Transient, attributes(covariant, contravariant, variance))]
// #[proc_macro_derive(Transient, attributes(variance))]
pub fn derive_transient(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as Input);
    generate_impls(input)
        .unwrap_or_else(|e| e.into_compile_error())
        .into()
}

// #[repr(C, align(4))]
// #[derive(Debug, Clone)]
// struct S {
//
// }

fn generate_impls(input: Input) -> Result<TokenStream2> {
    let generics = StaticBoundedGenerics::new(input.generics);
    let target_type = TypeWithGenerics::new(&input.name, &generics);
    let lifetimes = NonStaticLifetimes::extract(&generics)?;
    let mut variance_decls = VarianceDeclarations::extract(&input.attrs)?;

    if lifetimes.len() == 0 {
        variance_decls.ensure_empty()?;
        Ok(StaticImpl::construct(&target_type).into_token_stream())
    } else {
        let static_type = StaticType::new(&target_type);
        let transience = Transience::resolve(lifetimes, variance_decls)?;

        let transient_impl = TransientImpl::new(&target_type, &static_type, &transience);
        let checks_module = ChecksModule::new(&transient_impl);

        Ok(quote!(
            #transient_impl
            #checks_module
        ))
    }
}

struct StaticImpl<'a> {
    type_name: &'a Ident,
    impl_generics: ImplGenerics<'a>,
    type_generics: TypeGenerics<'a>,
    where_clause: Option<&'a WhereClause>,
}

impl<'a> StaticImpl<'a> {
    fn construct(target_type: &'a TypeWithGenerics<'a>) -> Self {
        let type_name = target_type.name;
        let (impl_generics, type_generics, where_clause) = target_type.generics.split_for_impl();
        StaticImpl {
            type_name,
            impl_generics,
            type_generics,
            where_clause,
        }
    }
}

impl<'a> ToTokens for StaticImpl<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let StaticImpl {
            type_name,
            impl_generics,
            type_generics,
            where_clause,
        } = self;
        quote!(
            impl #impl_generics ::transient::Static
            for #type_name #type_generics
            #where_clause {}
        )
        .to_tokens(tokens)
    }
}

struct TransientImpl<'a> {
    target_type: &'a TypeWithGenerics<'a>,
    impl_generics: ImplGenerics<'a>,
    where_clause: Option<&'a WhereClause>,
    static_type: &'a StaticType<'a>,
    transience: &'a Transience,
}

impl<'a> TransientImpl<'a> {
    fn new(
        target_type: &'a TypeWithGenerics<'a>,
        static_type: &'a StaticType<'a>,
        transience: &'a Transience,
    ) -> Self {
        let (impl_generics, _, where_clause) = target_type.generics.split_for_impl();
        TransientImpl {
            target_type,
            impl_generics,
            where_clause,
            static_type,
            transience,
        }
    }
}

impl<'a> ToTokens for TransientImpl<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TransientImpl {
            target_type,
            impl_generics,
            where_clause,
            static_type,
            transience,
        } = self;

        quote!(
            unsafe impl #impl_generics ::transient::Transient for #target_type #where_clause {
                type Static = #static_type;
                type Transience = #transience;
            }
        )
        .to_tokens(tokens)
    }
}

/// The type that is implementing the `Static` or `Transient` trait
#[derive(Clone)]
struct TypeWithGenerics<'a> {
    name: &'a Ident,
    generics: Cow<'a, Generics>,
}

impl<'a> TypeWithGenerics<'a> {
    fn new(name: &'a Ident, generics: &'a Generics) -> Self {
        TypeWithGenerics {
            name,
            generics: Cow::Borrowed(generics),
        }
    }

    fn owned(name: &'a Ident, generics: Generics) -> Self {
        TypeWithGenerics {
            name,
            generics: Cow::Owned(generics),
        }
    }
}

impl<'a> ToTokens for TypeWithGenerics<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TypeWithGenerics { name, generics } = self;
        let type_generics = generics.split_for_impl().1;
        quote!(#name #type_generics).to_tokens(tokens)
    }
}

/// The `Transient::Static` associated type; same as the implementing type
/// but with all lifetimes replaced by 'static
struct StaticType<'a> {
    name: &'a Ident,
    generics: Generics,
}

impl<'a> StaticType<'a> {
    fn new(target_type: &TypeWithGenerics<'a>) -> Self {
        let mut generics = target_type.generics.clone().into_owned();
        generics
            .lifetimes_mut()
            .for_each(|lt| *lt = parse_quote!('static));
        StaticType {
            name: target_type.name,
            generics,
        }
    }
}

impl<'a> ToTokens for StaticType<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let type_name = self.name;
        let (_, static_generics, _) = self.generics.split_for_impl();
        quote!(#type_name #static_generics).to_tokens(tokens)
    }
}

/// AST struct containing the relevant pieces from the `DeriveInput`
struct Input {
    name: Ident,
    generics: Generics,
    attrs: Vec<Attribute>,
}

impl Parse for Input {
    #[rustfmt::skip]
    fn parse(stream: ParseStream) -> syn::Result<Self> {
        let input = stream.parse::<DeriveInput>()?;
        let span = input.span();
        let DeriveInput { ident, generics, data, attrs, .. } = input;

        if let Data::Struct(DataStruct { .. }) = data {
            Ok(Input { name: ident, generics, attrs })
        } else {
            Err(Error::NotAStruct(span).into())
        }
    }
}

/// generics with a `'static` bound append to each type parameter
struct StaticBoundedGenerics(Generics);

impl StaticBoundedGenerics {
    fn new(mut generics: Generics) -> Self {
        generics
            .type_params_mut()
            .for_each(|param| param.bounds.push(parse_quote!('static)));
        StaticBoundedGenerics(generics)
    }
}

impl Deref for StaticBoundedGenerics {
    type Target = Generics;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

struct NonStaticLifetimes(Vec<Lifetime>);

impl NonStaticLifetimes {
    const MAX: usize = 4;

    fn extract(generics: &Generics) -> Result<Self> {
        let lifetimes = generics
            .lifetimes()
            .filter(|&lt| !lt.eq(&parse_quote!('static)))
            .map(|lt| lt.lifetime.clone())
            .collect::<Vec<_>>();
        if lifetimes.len() > Self::MAX {
            let span = lifetimes.into_iter().nth(Self::MAX).unwrap().span();
            Err(Error::TooManyLifetimes(span))
        } else {
            Ok(NonStaticLifetimes(lifetimes))
        }
    }
}

impl Deref for NonStaticLifetimes {
    type Target = Vec<Lifetime>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

struct VarianceDeclarations(BTreeMap<Lifetime, Variance>);

impl VarianceDeclarations {
    fn new() -> Self {
        VarianceDeclarations(BTreeMap::new())
    }
    /// collect all variance declarations from the struct attributes
    fn extract(attrs: &[Attribute]) -> Result<VarianceDeclarations> {
        let mut decls = VarianceDeclarations::new();
        attrs
            .into_iter()
            .filter(Self::filter_attr)
            .try_for_each(|attr| decls.parse_attr_into(attr))
            .map(|_| decls)
    }

    fn filter_attr(attr: &&Attribute) -> bool {
        attr.path().is_ident("invariant")
            || attr.path().is_ident("covariant")
            || attr.path().is_ident("contravariant")
    }

    /// parses from attribute content such as:
    ///   `covariant()`
    ///   `covariant(a)`
    ///   `contravariant(a, b, c)`
    fn parse_attr_into(&mut self, attr: &Attribute) -> Result<()> {
        let ident = attr.path().get_ident().unwrap();
        let kind = VarianceKind::from_ident(ident)?;
        attr.meta
            .require_list()?
            .parse_args_with(Punctuated::<Ident, Token![,]>::parse_terminated)?
            .into_iter()
            .try_for_each(|ident| {
                let lt = Lifetime {
                    apostrophe: ident.span(),
                    ident,
                };
                self.push(Variance::new(lt, kind.clone()))
            })
    }

    fn push(&mut self, variance: Variance) -> Result<()> {
        if let Some(old) = self.0.insert(variance.lifetime.clone(), variance) {
            Err(Error::DuplicateVariance {
                old: old.kind,
                new: self.0.pop_last().unwrap().1.kind,
            })
        } else {
            Ok(())
        }
    }

    fn pop_or_default(&mut self, lt: &Lifetime) -> Variance {
        self.0
            .remove(&lt)
            .unwrap_or_else(|| Variance::new(lt.clone(), VarianceKind::default()))
    }

    fn ensure_empty(&mut self) -> Result<()> {
        if self.0.is_empty() {
            Ok(())
        } else {
            let lifetime = self.0.pop_first().unwrap().1.lifetime;
            Err(Error::UnexpectedLifetime { lifetime })
        }
    }
}
use syn::punctuated::Punctuated;

struct VarianceAttr {
    kind: VarianceKind,
    lifetimes: Vec<Lifetime>,
}

impl VarianceAttr {
    fn check(attr: &Attribute) -> bool {
        attr.path().is_ident("invariant")
            || attr.path().is_ident("covariant")
            || attr.path().is_ident("contravariant")
    }
}

/*fn parse_attrs(attrs: &[Attribute]) -> Result<VarianceDeclarations> {
    let mut decls = VarianceDeclarations::new();
    attrs.iter()
        .filter(|attr| {
            attr.path().is_ident("covariant")
            || attr.path().is_ident("contravariant")
        })
        .map(|x| -> syn::Result<_> {
            let y = x.meta.require_list()?;
            let z = y.parse_args_with(
                Punctuated::<Ident, Token![,]>::parse_terminated
            )?
            ;
            Ok(())
        })
        // .filter(|attr| attr.path().is_ident("variance") )
        .map(|attr| attr.meta.require_list()?.parse_args_with(parse_variances))
        // .map(|attr| parse_variances_meta(&attr.meta))
        .collect::<syn::Result<Vec<_>>>()?
        .into_iter()
        .flatten()
        .try_for_each(|variance| decls.push(variance))?;
    Ok(decls)
}*/

#[derive(Clone, Debug)]
enum VarianceKind {
    Inv(Span),
    Co(Span),
    Contra(Span),
}

impl Default for VarianceKind {
    fn default() -> Self {
        VarianceKind::Inv(Span::call_site())
    }
}

#[allow(clippy::type_complexity)]
impl VarianceKind {
    /// mapping of valid keys to constructors
    const MAP: [(&'static str, fn(Span) -> VarianceKind); 6] = [
        ("inv", VarianceKind::Inv),
        ("invariant", VarianceKind::Inv),
        ("co", VarianceKind::Co),
        ("covariant", VarianceKind::Co),
        ("contra", VarianceKind::Contra),
        ("contravariant", VarianceKind::Contra),
    ];

    fn from_ident(ident: &Ident) -> Result<Self> {
        let span = ident.span();
        let mut string = ident.to_string();
        string.make_ascii_lowercase();
        if let Some((_, f)) = VarianceKind::MAP.iter().find(|(v, _)| v.eq(&string)) {
            Ok(f(span))
        } else {
            Err(Error::UnexpectedVariance { string, span }.into())
        }
    }

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
        Ok(Self::from_ident(&input.parse::<Ident>()?)?)
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
    lifetime: Lifetime,
    kind: VarianceKind,
}

impl Variance {
    fn new(lifetime: Lifetime, kind: VarianceKind) -> Self {
        Variance { lifetime, kind }
    }
}

impl Parse for Variance {
    // parses `'a = covariant`
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lifetime: Lifetime = input.parse()?;
        let _: Token![=] = input.parse()?;
        let kind: VarianceKind = input.parse()?;
        Ok(Variance { lifetime, kind })
    }
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

struct Transience2(Vec<Variance>);

impl Transience2 {
    fn resolve(
        lifetimes: NonStaticLifetimes,
        mut variance_decls: VarianceDeclarations,
    ) -> Result<Self> {
        // pop a variance from the declarations for each lifetime or use the default
        let variances = lifetimes
            .iter()
            .map(|lt| variance_decls.pop_or_default(lt))
            .collect::<Vec<_>>();
        // check for remaining declarations that correspond to invalid lifetimes
        variance_decls.ensure_empty()?;
        Ok(Transience2(variances))
    }
}

enum Transience {
    Variance(Variance),
    Tuple(Vec<Variance>),
}

impl Transience {
    fn resolve(
        lifetimes: NonStaticLifetimes,
        mut variance_decls: VarianceDeclarations,
    ) -> Result<Self> {
        // pop a variance from the declarations for each lifetime or use the default
        let mut variances = lifetimes
            .iter()
            .map(|lt| variance_decls.pop_or_default(lt))
            .collect::<Vec<_>>();

        // check for remaining declarations that correspond to invalid lifetimes
        variance_decls.ensure_empty()?;

        // assembly variances into the final transience enum
        match variances.len() {
            0 => unreachable!("this should not be called for static structs"),
            1 => Ok(Transience::Variance(variances.pop().unwrap())),
            _ => Ok(Transience::Tuple(variances)),
        }
    }

    fn variances(&self) -> &[Variance] {
        match self {
            Self::Variance(variance) => std::slice::from_ref(variance),
            Self::Tuple(variances) => variances,
        }
    }
}

impl ToTokens for Transience {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let variances = self.variances();
        if variances.len() == 1 {
            variances[0].to_tokens(tokens)
        } else {
            quote!((#(#variances,)*)).to_tokens(tokens)
        }
        /*match self {
            Self::Variance(v) => v.to_tokens(tokens),
            Self::Tuple(vs) => quote!((#(#vs,)*)).to_tokens(tokens),
        }*/
    }
}

/// module generated to force a compile error if an invalid variance is selected
struct ChecksModule<'a> {
    name: Ident,
    funcs: Vec<CheckFn<'a>>,
}

impl<'a> ChecksModule<'a> {
    fn new(impl_: &'a TransientImpl<'a>) -> Self {
        let target_type = &impl_.target_type;
        let name = Ident::new(
            &format!("__validate_{}", target_type.name),
            target_type.name.span(),
        );
        let funcs = impl_
            .transience
            .variances()
            .into_iter()
            .filter_map(|variance| CheckFn::new(target_type, variance))
            .collect::<Vec<_>>();
        ChecksModule { name, funcs }
    }
}

impl<'a> ToTokens for ChecksModule<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ChecksModule { name, funcs } = self;
        if funcs.len() == 0 {
            return ();
        }
        quote!(
            #[allow(non_snake_case, dead_code)]
            mod #name {
                use super::*;
                #(#funcs)*
            }
        )
        .to_tokens(tokens)
    }
}

struct CheckFn<'a> {
    func_name: Ident,
    // implementing type with the test lifetime appended to its generics
    arg_type: TypeWithGenerics<'a>,
    // same as the implementing type
    return_type: &'a TypeWithGenerics<'a>,
    // generics and where clause for the function
    generics: Generics,
}
impl<'a> CheckFn<'a> {
    fn new(target_type: &'a TypeWithGenerics, variance: &'a Variance) -> Option<CheckFn<'a>> {
        let lt = &variance.lifetime;
        let test_lt: LifetimeParam;
        let func_name: String;
        let where_pred: WherePredicate;
        match variance.kind {
            VarianceKind::Inv(_) => return None,
            VarianceKind::Co(span) => {
                test_lt = LifetimeParam::new(Lifetime::new("'__long", span));
                func_name = format!("covariant_wrt_{}", lt.ident);
                where_pred = parse_quote!(#test_lt: #lt)
            }
            VarianceKind::Contra(span) => {
                test_lt = LifetimeParam::new(Lifetime::new("'__short", span));
                func_name = format!("contravariant_wrt_{}", lt.ident);
                where_pred = parse_quote!(#lt: #test_lt)
            }
        }

        // generics for the func; original with the test lifetime inserted
        let mut func_generics = target_type.generics.as_ref().clone();
        func_generics
            .params
            .insert(0, GenericParam::Lifetime(test_lt.clone()));
        func_generics
            .make_where_clause()
            .predicates
            .push(where_pred);

        // generics for the funcs argument; original with 'a replaced by 'test
        let mut arg_generics = target_type.generics.as_ref().clone();
        arg_generics
            .lifetimes_mut()
            .find(|param| param.lifetime.eq(&lt))
            .map(|param| *param = test_lt)?;

        Some(CheckFn {
            func_name: Ident::new(&func_name, variance.span()),
            arg_type: TypeWithGenerics::owned(target_type.name, arg_generics),
            return_type: target_type,
            generics: func_generics,
        })
    }
}

impl<'a> ToTokens for CheckFn<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let CheckFn {
            func_name,
            arg_type,
            return_type,
            generics,
        } = self;
        let (func_generics, _, where_clause) = generics.split_for_impl();
        quote_spanned!(func_name.span() =>
            fn #func_name #func_generics(v: #arg_type) -> #return_type
            #where_clause
            {
                v
            }
        )
        .to_tokens(tokens)
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error(transparent)]
    Syn(#[from] syn::Error),
    #[error("Only `struct`'s are supported!")]
    NotAStruct(Span),
    #[error("At most {} lifetime parameters are allowed!", NonStaticLifetimes::MAX)]
    TooManyLifetimes(Span),
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
        "Variance declared for an invalid lifetime '{}'!\n ",
        lifetime.to_string()
    )]
    UnexpectedLifetime { lifetime: Lifetime },
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
            Error::DuplicateVariance { new, .. } => new.span(),
            Error::UnexpectedVariance { span, .. } => *span,
            Error::UnexpectedLifetime { lifetime } => lifetime.span(),
        }
    }
}

impl From<Error> for syn::Error {
    fn from(value: Error) -> Self {
        syn::Error::new(value.span(), value.to_string())
    }
}

impl<'a> ChecksModule<'a> {
    fn construct(type_name: &'a Ident, transience: &'a Transience, generics: &'a Generics) -> Self {
        let name = format!("__validate_{}", type_name.to_string());
        ChecksModule {
            name: Ident::new(&name, Span::call_site()),
            funcs: transience
                .as_slice()
                .iter()
                .filter_map(|v| construct_check(type_name, v, generics))
                .collect(),
        }
    }
}

impl<'a> ToTokens for ChecksModule<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ChecksModule { name, funcs } = self;
        if funcs.len() == 0 {
            return ();
        }
        quote!(
            #[allow(non_snake_case, dead_code)]
            mod #name {
                use super::*;
                #(#funcs)*
            }
        )
        .to_tokens(tokens)
    }
}

/// ```skip
/// struct TypeAndLifetime<'a, 'b, T>(fn(&'b T) -> &'a T) where T: Clone;
///
/// #[allow(non_snake_case)]
/// mod __validate_TypeAndLifetime {
///     use super::TypeAndLifetime;
///
///     fn covariant_wrt_a<'__long, 'a, 'b, T>(
///         v: TypeAndLifetime<'__long, 'b, T>,
///     ) -> TypeAndLifetime<'a, 'b, T>
///     where '__long: 'a, T: Clone + 'static
///     {
///         v
///     }
///     fn contravariant_wrt_b<'__short, 'a, 'b, T>(
///         v: TypeAndLifetime<'a, '__short, T>,
///     ) -> TypeAndLifetime<'a, 'b, T>
///     where 'b: '__short, T: Clone + 'static
///     {
///         v
///     }
/// }
/// ```
struct CheckFn<'a> {
    type_name: &'a Ident,
    func_name: Ident,
    // generics and where clause for the function
    func_generics: Generics,
    // generics for the type in the func arg
    arg_generics: Generics,
    // return value generics
    return_generics: TypeGenerics<'a>,
}

impl<'a> ToTokens for CheckFn<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let CheckFn {
            type_name,
            func_name,
            func_generics,
            arg_generics,
            return_generics,
        } = self;
        let (func_generics, _, where_clause) = func_generics.split_for_impl();
        let (_, arg_generics, _) = arg_generics.split_for_impl();
        quote!(
            fn #func_name #func_generics(
                v: #type_name #arg_generics,
            ) -> #type_name #return_generics #where_clause {
                v
            }
        )
        .to_tokens(tokens)
    }
}

fn construct_check<'a>(
    type_name: &'a Ident,
    variance: &'a Variance,
    generics: &'a Generics,
) -> Option<CheckFn<'a>> {
    let test_lt: Lifetime;
    let func_name: String;
    let where_pred: WherePredicate;

    let lt = &variance.lifetime;
    let lt_name = lt.ident.to_string();

    match variance.kind {
        VarianceKind::Inv(_) => return None,
        VarianceKind::Co(_) => {
            test_lt = Lifetime::new("'__long", Span::call_site());
            func_name = format!("covariant_wrt_{lt_name}");
            where_pred = parse_quote!(#test_lt: #lt)
        }
        VarianceKind::Contra(_) => {
            test_lt = Lifetime::new("'__short", Span::call_site());
            func_name = format!("contravariant_wrt_{lt_name}");
            where_pred = parse_quote!(#lt: #test_lt)
        }
    }

    // generics for the func; original with the test lifetime inserted
    let func_generics = generics
        .clone()
        .insert_lifetime(0, LifetimeParam::new(test_lt.clone()))
        .insert_where_predicate(where_pred);

    // generics for the funcs argument; original with 'a replaced by 'test
    let arg_generics = generics
        .clone()
        .swap_lifetime(&lt, LifetimeParam::new(test_lt))
        .expect("lifetime not found");

    // generics for the return value; same as original
    let return_generics = generics.split_for_impl().1;

    Some(CheckFn {
        type_name,
        func_name: Ident::new(&func_name, Span::call_site()),
        func_generics,
        arg_generics,
        return_generics,
    })
}

trait GenericsTransform: Sized + 'static {
    fn insert_where_predicate(self, pred: WherePredicate) -> Generics;
    fn swap_lifetime(self, old: &Lifetime, new: LifetimeParam) -> Option<Generics>;
    fn insert_lifetime(self, index: usize, new: LifetimeParam) -> Generics;
}

impl GenericsTransform for Generics {
    fn insert_where_predicate(mut self, pred: WherePredicate) -> Generics {
        self.make_where_clause().predicates.push(pred);
        self
    }

    fn swap_lifetime(mut self, old: &Lifetime, new: LifetimeParam) -> Option<Generics> {
        self.lifetimes_mut()
            .find(|lt| lt.lifetime.eq(old))
            .map(|lt| *lt = new)?;
        Some(self)
    }

    fn insert_lifetime(mut self, index: usize, new: LifetimeParam) -> Generics {
        self.params.insert(index, GenericParam::Lifetime(new));
        self
    }
}
