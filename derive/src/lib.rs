//! Defines the [`Transient`][crate::Transient] derive macro that safely implements
//! the [`Transient`][transient::tr::Transient] trait for any struct with at most 4
//! lifetime parameters.
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote_spanned};
use syn::{Data, DeriveInput, Generics, Ident, Lifetime};

mod checks;
mod options;
mod utils;
mod variance;

use checks::ChecksModule;
use options::TransientOptions;
use utils::{extract_lifetimes, insert_static_predicates, TypeWithGenerics};
use variance::{Variance, VarianceDeclarations, VarianceKind};

// Maximum tuple length for which `Transience` is implemented
const MAX_LIFETIMES: usize = 5;

/// Derive macro that implements the  [`Transient`] trait for any struct.
///
/// Note that when this macro is applied to a `'static` struct with no lifetime
/// parameters, it will instead implement the [`Static`] trait which results in
/// a blanket impl of the `Transient` trait.
///
/// This macro is limited to structs satisfying the following conditions:
/// - There must be at most 5 lifetime parameters. This is not a limitation of
///   the derive macro, but in the tuple length for which the `Transience` trait
///   is implemented.
/// - There may be any number of type (or const) parameters, but the trait will
///   only be implemented where `T: 'static` for each type parameter `T`.
///
/// # Customization
///
/// ## Variance
/// By default, the [variance] of a deriving struct is assumed to be _invariant_
/// respect to its lifetime parameter (if it has one), since this is the only
/// type of variance that can be safely used for _all_ types without analyzing
/// the behavior of its fields (which this macro does not attempt to do). When
/// the added flexibility of _covariance_ or _contravariance_ is needed, the
/// `covariant` and `contravariant` helper attributes can be used to override
/// this default. When these attributes are used, a test module will be generated
/// which defines a function for each lifetime that will fail to compile if the
/// requested variance is not appropriate for the type.
///
/// One or more of these attributes may declared in the following forms:
/// 1. `#[covariant]` or `#[contravariant]`
/// 2. `#[covariant()]` or `#[contravariant()]`
/// 3. `#[covariant(a)]` or `#[contravariant(a)]`
/// 4. `#[covariant(a, b, c)]` or `#[contravariant(a, b, c)]`
///
/// Option 1 is a global declaration of the variance for ALL lifetimes, and must be
/// the only declaration. Option 2 is a no-op, but still conflicts with the first
/// option. Option 3 declares the variance for the type w.r.t. `'a`, and can coexist
/// with other (non-global) declarations as long as there is no overlap. Option 4
/// declares the variance w.r.t. each of the listed lifetimes, subject to the same
/// rules as for option 3.
///
/// ## Crate path
/// If it is desired that the `transient` crate's symbols be referenced from an
/// alternate path in the generated impl, such as a re-export of the symbols in a
/// downstream library, the `#[transient(crate = path::to::transient)]` attribute
/// can be applied to the struct. By default, the path will be `::transient`.
///
/// # Failure modes
/// This macro can fail for any of the following reasons:
/// - The declared variance is not valid for the type. The compiler error generated
///   in this case is not particularly helpful, but can be recognized by suggestions
///   such as "help: consider adding the following bound: `'a: '__long`".
/// - Requesting any variance for a type with no lifetime parameters
/// - Requesting a variance for a lifetime that does not appear on the struct
/// - Declaring a "global" variance in addition to a lifetime-specific variance
/// - Providing more than one "variance" attribute for a lifetime parameter
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
/// unsafe impl<'a, T> ::transient::Transient for S<'a, T>
/// where
///     T: 'static,
/// {
///     type Static = S<'static, T>;
///     type Transience = transient::Inv<'a>;
/// }
/// ```
///
/// # Example - struct with two lifetime parameters and a _covariance_ declaration
///
/// Invocation:
/// ```
/// use transient::Transient;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Transient)]
/// #[covariant(a)]
/// struct S<'a, 'b> {
///     name: &'a String,  // declared covariant
///     values: &'b [i32], // no variance specified, defaults to invariant
/// }
/// # fn main() {}
/// ```
///
/// Generated impl:
/// ```
/// # struct S<'a, 'b> {name: &'a String, values: &'b [i32]}
/// unsafe impl<'a, 'b> ::transient::Transient for S<'a, 'b> {
///     type Static = S<'static, 'static>;
///     type Transience = (transient::Co<'a>, transient::Inv<'a>);
/// }
/// ```
///
/// This invocation also generates a test module, not shown above, including a
/// function that will fail to compile if the declared variance is not correct
/// for the type.
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
/// impl<T: 'static> transient::Static for S<T>
/// where
///     T: Clone + 'static,
/// {}
/// ```
///
/// [`Transient`]: ../transient/trait.Transient.html
/// [`Static`]: ../transient/trait.Static.html
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
#[proc_macro_derive(Transient, attributes(transient, covariant, contravariant))]
pub fn derive_transient(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match generate_impls(input) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

fn generate_impls(input: DeriveInput) -> Result<TokenStream2> {
    if !matches!(input.data, Data::Struct(_)) {
        return Err(Error::NotAStruct(input.ident.span()));
    }
    let options = TransientOptions::extract(&input.attrs)?;
    let mut variance_decls = VarianceDeclarations::extract(&input.attrs)?;
    let self_type = SelfType::new(input.ident, input.generics)?;
    if self_type.is_static() {
        variance_decls.ensure_empty()?;
        Ok(StaticImpl(&self_type, &options).into_token_stream())
    } else {
        let static_type = self_type.get_static_type();
        let transience = self_type.resolve_transience(variance_decls, &options)?;
        let transient_impl = TransientImpl(&self_type, static_type, transience, &options);
        let checks_module = ChecksModule::new(&transient_impl);
        Ok(quote!(#transient_impl #checks_module))
    }
}

/// Represents the `impl Static` block for a 'static type
struct StaticImpl<'a>(&'a SelfType, &'a TransientOptions);

impl<'a> ToTokens for StaticImpl<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let StaticImpl(self_type, TransientOptions { krate }) = self;
        let (impl_generics, _, where_clause) = self_type.generics.split_for_impl();
        tokens.extend(quote!(
            impl #impl_generics #krate::Static for #self_type #where_clause {}
        ));
    }
}

/// Represents the `impl Transient` block for a non-'static type
struct TransientImpl<'a>(
    &'a SelfType,
    StaticType<'a>,
    Transience<'a>,
    &'a TransientOptions,
);

impl<'a> ToTokens for TransientImpl<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TransientImpl(self_type, static_type, transience, options) = self;
        let krate = &options.krate;
        let (impl_generics, _, where_clause) = self_type.generics.split_for_impl();
        tokens.extend(quote!(
            unsafe impl #impl_generics #krate::Transient for #self_type #where_clause {
                type Static = #static_type;
                type Transience = #transience;
            }
        ));
    }
}

/// The target type implementing the `Static` or `Transient` trait
struct SelfType {
    name: Ident,
    generics: Generics,
    lifetimes: Vec<Lifetime>,
}

impl SelfType {
    fn new(name: Ident, mut generics: Generics) -> Result<Self> {
        insert_static_predicates(&mut generics);
        let lifetimes = extract_lifetimes(&generics);
        if lifetimes.len() > MAX_LIFETIMES {
            let extra = lifetimes.into_iter().nth(MAX_LIFETIMES).unwrap();
            return Err(Error::TooManyLifetimes(extra.span()));
        }
        Ok(SelfType { name, generics, lifetimes })
    }
    /// Query whether the type is 'static
    fn is_static(&self) -> bool {
        self.lifetimes.is_empty()
    }
    /// Get the `Static` associated type by replacing lifetimes with 'static
    fn get_static_type(&self) -> StaticType<'_> {
        let mut generics = self.generics.clone();
        generics
            .lifetimes_mut()
            .for_each(|lt| *lt = parse_quote_spanned!(lt.span() => 'static));
        StaticType { name: &self.name, generics }
    }
    /// Attempt to unify the lifetimes of the type and the variances declared in its
    /// attributes to establish the variance with respect to each lifetime.
    fn resolve_transience<'a>(
        &'a self,
        mut decls: VarianceDeclarations,
        options: &'a TransientOptions,
    ) -> Result<Transience<'a>> {
        // pop a variance from the declarations for each lifetime or use the default
        let variances = self
            .lifetimes
            .iter()
            .map(|lt| decls.pop_variance(lt, options))
            .collect::<Vec<_>>();
        // check for remaining declarations that correspond to invalid lifetimes
        decls.ensure_empty()?;
        Ok(Transience(variances))
    }
}

impl ToTokens for SelfType {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let Self { name, generics, .. } = self;
        let type_generics = generics.split_for_impl().1;
        quote!(#name #type_generics).to_tokens(tokens);
    }
}

/// The `Transient::Static` associated type
type StaticType<'a> = TypeWithGenerics<'a>;

/// The `Transient::Transience` associated type
struct Transience<'a>(Vec<Variance<'a>>);

impl<'a> ToTokens for Transience<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let variances = &self.0;
        if variances.len() == 1 {
            variances[0].to_tokens(tokens);
        } else {
            quote!((#(#variances,)*)).to_tokens(tokens);
        };
    }
}

/// Collection of internal error conditions
#[derive(Debug, thiserror::Error)]
pub(crate) enum Error {
    #[error(transparent)]
    Syn(#[from] syn::Error),
    #[error("Only `struct`'s are supported!")]
    NotAStruct(Span),
    #[error("At most {} lifetime parameters are allowed!", MAX_LIFETIMES)]
    TooManyLifetimes(Span),
    #[error("Duplicate variance specification! '{0}' replaced by '{1}'")]
    DuplicateVariance(VarianceKind, VarianceKind),
    #[error("Variance declared for an invalid lifetime `'{0}`!")]
    UnexpectedLifetime(Ident),
    #[error("Invalid option!")]
    InvalidOption(Span),
}

pub(crate) type Result<T> = std::result::Result<T, Error>;

impl Error {
    fn into_compile_error(self) -> TokenStream2 {
        syn::Error::from(self).into_compile_error()
    }
    fn span(&self) -> Span {
        match self {
            Self::Syn(err) => err.span(),
            Self::DuplicateVariance(_, new) => new.span(),
            Self::UnexpectedLifetime(lifetime) => lifetime.span(),
            Self::NotAStruct(span) | Self::TooManyLifetimes(span) | Self::InvalidOption(span) => {
                *span
            }
        }
    }
}

impl From<Error> for syn::Error {
    fn from(value: Error) -> Self {
        match value {
            Error::Syn(err) => err,
            err => Self::new(err.span(), err.to_string()),
        }
    }
}
