//! Defines the [`Transient`][crate::Transient] derive macro that safely implements
//! the [`Transient`][transient::tr::Transient] trait for any struct with at most 4
//! lifetime parameters.
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned, ToTokens};
use std::{collections::BTreeMap, fmt, mem, ops::Deref};
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{parse_macro_input, parse_quote, parse_quote_spanned};
use syn::{Attribute, Data, DeriveInput, Ident, Lifetime, Meta, Token};
use syn::{GenericParam, Generics, LifetimeParam, WherePredicate};

// Maximum tuple length for which `Transience` is implemented
const MAX_LIFETIMES: usize = 4;

/// Derive macro that implements the  [`Transient`] trait for any struct.
///
/// Note that when this macro is applied to a `'static` struct with no lifetime
/// parameters, it will instead implement the [`Static`] trait which results in
/// a blanket impl of the `Transient` trait.
///
/// This macro is limited to structs satisfying the following conditions:
/// - There must be at most 4 lifetime parameters. This is not a limitation of
///   the derive macro, but in the tuple length for which the `Transience` trait
///   is implemented.
/// - There may be any number of type (or const) parameters, but the trait will
///   only be implemented where `T: 'static` for each type parameter `T`.
///
/// # Customization
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
#[proc_macro_derive(Transient, attributes(covariant, contravariant))]
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
    let mut variance_decls = VarianceDeclarations::extract(&input.attrs)?;
    let self_type = SelfType::new(&input.ident, input.generics);
    let lifetimes = self_type.extract_lifetimes()?;
    if lifetimes.is_empty() {
        variance_decls.ensure_empty()?;
        Ok(StaticImpl(self_type).into_token_stream())
    } else {
        let static_type = self_type.get_static_type();
        let transience = Transience::resolve(lifetimes, variance_decls)?;
        let transient_impl = TransientImpl::new(self_type, static_type, transience);
        let checks_module = ChecksModule::new(&transient_impl);
        Ok(quote!(#transient_impl #checks_module))
    }
}

struct StaticImpl<'a>(SelfType<'a>);

impl<'a> ToTokens for StaticImpl<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let target_type = &self.0;
        let (impl_generics, _, where_clause) = target_type.generics.split_for_impl();
        tokens.extend(quote!(
            impl #impl_generics ::transient::Static for #target_type #where_clause {}
        ));
    }
}

struct TransientImpl<'a> {
    target_type: SelfType<'a>,
    static_type: TypeWithGenerics<'a>,
    transience: Transience,
}

impl<'a> TransientImpl<'a> {
    fn new(
        target_type: SelfType<'a>,
        static_type: TypeWithGenerics<'a>,
        transience: Transience,
    ) -> Self {
        TransientImpl {
            target_type,
            static_type,
            transience,
        }
    }
}

impl<'a> ToTokens for TransientImpl<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TransientImpl {
            target_type,
            static_type,
            transience,
        } = self;
        let (impl_generics, _, where_clause) = target_type.generics.split_for_impl();
        tokens.extend(quote!(
            unsafe impl #impl_generics ::transient::Transient for #target_type #where_clause {
                type Static = #static_type;
                type Transience = #transience;
            }
        ));
    }
}

/// The target type implementing the trait
struct SelfType<'a>(TypeWithGenerics<'a>);

impl<'a> SelfType<'a> {
    fn new(name: &'a Ident, mut generics: Generics) -> Self {
        insert_static_predicates(&mut generics);
        SelfType(TypeWithGenerics { name, generics })
    }

    /// get the `Static` associated type by replacing lifetimes with 'static
    fn get_static_type(&self) -> TypeWithGenerics<'a> {
        let mut generics = self.generics.clone();
        generics
            .lifetimes_mut()
            .for_each(|lt| *lt = parse_quote_spanned!(lt.span() => 'static));
        TypeWithGenerics {
            name: self.name,
            generics,
        }
    }

    /// extract the lifetimes parameters and validate the count
    fn extract_lifetimes(&self) -> Result<Vec<Lifetime>> {
        let lifetimes = self
            .generics
            .lifetimes()
            .filter(|lt| lt.lifetime.ident != "static")
            .map(|lt| lt.lifetime.clone())
            .collect::<Vec<_>>();
        if lifetimes.len() > MAX_LIFETIMES {
            let extra = lifetimes.into_iter().nth(MAX_LIFETIMES).unwrap();
            Err(Error::TooManyLifetimes(extra.span()))
        } else {
            Ok(lifetimes)
        }
    }
}

impl<'a> Deref for SelfType<'a> {
    type Target = TypeWithGenerics<'a>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> ToTokens for SelfType<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.deref().to_tokens(tokens);
    }
}

struct TypeWithGenerics<'a> {
    name: &'a Ident,
    generics: Generics,
}

impl<'a> ToTokens for TypeWithGenerics<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let TypeWithGenerics { name, generics } = self;
        let type_generics = generics.split_for_impl().1;
        quote!(#name #type_generics).to_tokens(tokens);
    }
}

/// Represents the set of variances declared in the macro attributes.
enum VarianceDeclarations {
    // initial state with no variances declared
    Empty,
    // state after a global declaration
    Global(VarianceKind),
    // state after 1+ specific declarations
    Mapping(BTreeMap<Ident, VarianceKind>),
}

impl VarianceDeclarations {
    fn extract(attrs: &[Attribute]) -> Result<Self> {
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
            let Self::Global(old) = mem::replace(self, Self::Empty) else {
                unreachable!()
            };
            return Err(Error::DuplicateVariance(old, kind));
        }
        // this attr declares a global variance, so there must be no existing decls
        if let Meta::Path(_) = attr.meta {
            let old = match mem::replace(self, Self::Empty) {
                Self::Global(old) => old,
                Self::Mapping(mut map) => map.pop_last().unwrap().1,
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
            let new = map.pop_last().unwrap().1;
            Err(Error::DuplicateVariance(old, new))
        } else {
            Ok(())
        }
    }

    fn pop_variance(&mut self, lifetime: Lifetime) -> Variance {
        let kind = match self {
            Self::Empty => VarianceKind::default(),
            Self::Global(kind) => kind.clone(),
            Self::Mapping(map) => map.remove(&lifetime.ident).unwrap_or_default(),
        };
        Variance { lifetime, kind }
    }

    fn ensure_empty(&mut self) -> Result<()> {
        match self {
            Self::Mapping(map) if !map.is_empty() => {
                let lifetime = map.pop_first().unwrap().0;
                Err(Error::UnexpectedLifetime(lifetime))
            }
            _ => Ok(()),
        }
    }
}

#[derive(Clone, Debug)]
enum VarianceKind {
    Inv(Span),
    Co(Span),
    Contra(Span),
}

impl VarianceKind {
    fn span(&self) -> Span {
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

struct Variance {
    lifetime: Lifetime,
    kind: VarianceKind,
}

impl ToTokens for Variance {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        use VarianceKind::*;
        let lt = &self.lifetime;
        tokens.extend(match self.kind {
            Inv(span) => quote_spanned!(span => ::transient::Inv<#lt>),
            Co(span) => quote_spanned!(span => ::transient::Co<#lt>),
            Contra(span) => quote_spanned!(span => ::transient::Contra<#lt>),
        });
    }
}

struct Transience(Vec<Variance>);

impl Transience {
    fn resolve(lifetimes: Vec<Lifetime>, mut decls: VarianceDeclarations) -> Result<Self> {
        // pop a variance from the declarations for each lifetime or use the default
        let variances = lifetimes
            .into_iter()
            .map(|lt| decls.pop_variance(lt))
            .collect::<Vec<_>>();
        // check for remaining declarations that correspond to invalid lifetimes
        decls.ensure_empty()?;
        Ok(Transience(variances))
    }
    fn iter(&self) -> std::slice::Iter<Variance> {
        self.0.iter()
    }
}

impl ToTokens for Transience {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let variances = &self.0;
        if variances.len() == 1 {
            variances[0].to_tokens(tokens);
        } else {
            quote!((#(#variances,)*)).to_tokens(tokens);
        };
    }
}

/// module generated to force a compile error if an invalid variance is selected
struct ChecksModule<'a> {
    name: Ident,
    type_name: &'a Ident,
    funcs: Vec<CheckFn<'a>>,
}

impl<'a> ChecksModule<'a> {
    #[rustfmt::skip]
    fn new(impl_: &'a TransientImpl<'a>) -> Option<Self> {
        let target_type = &impl_.target_type;
        let type_name = impl_.target_type.name;
        let name = Ident::new(
            &format!("__validate_{type_name}"),
            target_type.name.span(),
        );
        let funcs = impl_.transience.iter()
            .filter_map(|variance| CheckFn::new(target_type, variance))
            .collect::<Vec<_>>();
        if funcs.is_empty() {
            None
        } else {
            Some(ChecksModule { name, type_name, funcs })
        }
    }
}

impl<'a> ToTokens for ChecksModule<'a> {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        let ChecksModule {
            name,
            funcs,
            type_name,
        } = self;
        tokens.extend(quote!(
            #[allow(non_snake_case, dead_code)]
            mod #name {
                use super::#type_name;
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
    return_type: &'a SelfType<'a>,
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
                name: target_type.name,
                generics: arg_generics,
            },
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
        tokens.extend(quote!(
            fn #func_name #func_generics(v: #arg_type) -> #return_type
            #where_clause
            {
                v
            }
        ));
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, thiserror::Error)]
enum Error {
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
}

impl Error {
    fn into_compile_error(self) -> TokenStream2 {
        syn::Error::from(self).into_compile_error()
    }
    fn span(&self) -> Span {
        match self {
            Self::Syn(err) => err.span(),
            Self::DuplicateVariance(_, new) => new.span(),
            Self::UnexpectedLifetime(lifetime) => lifetime.span(),
            Self::NotAStruct(span) | Self::TooManyLifetimes(span) => *span,
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

mod utils {
    use std::collections::BTreeSet;
    use syn::punctuated::Iter;
    use syn::{parse_quote, Generics, Ident, Type};
    use syn::{GenericParam, PredicateType, TypeParam, TypeParamBound, WherePredicate};

    /// Adds a `'static` bound to the `where` clause of the given generics for each 
    /// type parameter, skipping those that already have a 'static bound
    #[rustfmt::skip]
    pub(super) fn insert_static_predicates(generics: &mut Generics) {
        // Add where clause if there isn't one a get a mutable reference to it. We 
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
                bounds.push(parse_quote!('static));
            });
        // Add bound for any remaining params that were missed in the last pass since 
        // they did not have an existing where clause predicate
        for ident in &needs_bound {
            where_clause.predicates.push(parse_quote!(#ident: 'static));
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
    fn predicate_as_type(pred: &mut WherePredicate) -> Option<&mut PredicateType> {
        if let WherePredicate::Type(tp) = pred {
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
}
use utils::insert_static_predicates;
