//! Defines the [`Transient`][crate::Transient] derive macro that implements the
//! [`Transient`][transient::tr::Transient] trait for a struct with at most 1 
//! lifetime parameter.

use std::fmt;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, Lifetime, DeriveInput, Result, Error, 
    Generics, TypeGenerics, WhereClause, GenericParam, TypeParamBound, 
    Path, Attribute, Ident, Data, Fields, spanned::Spanned, 
};


/// Derive macro that implements the  [`Transient`] trait for a struct with
/// at most 1 lifetime parameter.
///
/// This macro is limited to structs satisfying the following conditions:
/// - There must be at most 1 lifetime parameter. Structs with extra lifetime
/// parameters can easily implement the trait by hand, but care must be taken
/// to ensure that the invariants detailed in the trait's [safety docs] are
/// upheld.
/// - There may be any number of type (or const) parameters, but the trait
/// will only be implemented where `T: 'static` for each type parameter `T`.
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
/// # Examples
/// Invocation with a type parameter and a lifetime parameter:
/// ```no_run
/// use transient::Transient;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Transient)]
/// struct S<'a, T> {
///     value: &'a T,
/// }
/// ```
/// This will generate the following impl:
/// ```
/// # struct S<'a, T> {value: &'a T}
/// unsafe impl<'a, T: 'static> transient::Transient for S<'a, T> {
///     type Static = S<'static, T>;
///     type Transience = transient::Inv<'a>;
/// }
/// ```
/// 
/// Invocation with a single lifetime and an attribute declaring _covariance_:
/// ```no_run
/// use transient::Transient;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Transient)]
/// struct S<'a> {
///     #[variance(unsafe_co)]
///     name: String,
///     values: &'a [i32]
/// }
/// ```
/// The generated impl will then be:
/// ```
/// # struct S<'a> {name: String, values: &'a [i32]}
/// unsafe impl<'a> transient::Transient for S<'a> {
///     type Static = S<'static>;
///     type Transience = transient::Co<'a>;
/// }
/// ```
/// 
/// [`Transient`]: ../transient/trait.Transient.html
/// [safety docs]: ../transient/trait.Transient.html#safety
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
#[proc_macro_derive(Transient, attributes(variance))]
pub fn derive_transient(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let tokens = generate_impl(input)
        .unwrap_or_else(|e| e.into_compile_error());
    TokenStream::from(tokens)
}

fn generate_impl(input: DeriveInput) -> Result<TokenStream2> {
    let span = input.span();
    let name = &input.ident;

    let params = process_generics(input.generics)?;
    let variance =  parse_struct(&input.data, params.is_static(), span)?;
    
    let impl_generics = params.impl_generics();
    let (ty_generics, where_clause) = params.split_for_impl();
    let static_ty_generics = params.static_type_generics();
    let transience_generics = params.transience_generics();
    
    let tokens = quote!(
        unsafe impl #impl_generics ::transient::Transient for #name #ty_generics
        #where_clause {
            type Static = #name #static_ty_generics;
            type Transience = #variance #transience_generics;
        }
    );
    Ok(tokens)
}

const VALID_VARIANCES: [&'static str; 6] = [
    "inv", "invariant",
    "unsafe_co", "unsafe_covariant",
    "unsafe_contra", "unsafe_contravariant",
];

#[derive(Debug)]
enum VarianceKind {
    Covariant,
    Contravariant,
    Invariant,
    Static,
}

impl VarianceKind {
    fn spanned(self, span: Span) -> Variance {
        Variance(self, Some(span))
    }
    fn unspanned(self) -> Variance {
        Variance(self, None)
    }
    
    fn as_path(&self) -> Path {
        match self {
            VarianceKind::Invariant => parse_quote!(::transient::Inv),
            VarianceKind::Covariant => parse_quote!(::transient::Co),
            VarianceKind::Contravariant => parse_quote!(::transient::Contra),
            VarianceKind::Static => parse_quote!(::transient::Timeless),
        }
    }
}

#[derive(Debug)]
struct Variance(VarianceKind, Option<Span>);

impl Variance {    
    fn span(&self) -> Span {
       match self.1.as_ref() {
           Some(span) => span.clone(),
           None => Spanned::span(self)
       } 
    }
    
    fn from_ident(id: &Ident) -> Result<Self> {
        let mut string = id.to_string();
        string.make_ascii_lowercase();
        match string.as_str() {
            "inv" | "invariant" => Ok(VarianceKind::Invariant),
            "unsafe_co" | "unsafe_covariant" => Ok(VarianceKind::Covariant),
            "unsafe_contra" | "unsafe_contravariant" => Ok(VarianceKind::Contravariant),
            "co" | "covariant" | "contra" | "contravariant" => { 
                Err(unsafe_variance_err(&string, id.span())) 
            },
            _ => Err(unexpected_variance_err(&string, id.span())),
        }.map(|s| s.spanned(id.span()))
    }
}

impl ToTokens for Variance {
    fn to_tokens(&self, tokens: &mut TokenStream2) {
        self.0.as_path().to_tokens(tokens);
    }
}

impl fmt::Display for Variance {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self.0 {            
            VarianceKind::Invariant => "invariant",
            VarianceKind::Covariant => "covariant",
            VarianceKind::Contravariant => "contravariant",
            VarianceKind::Static => "static",
        };
        fmt::Display::fmt(s, f)
    }
}

fn parse_struct(data: &Data, is_static: bool, span: Span) -> Result<Variance> {
    let Data::Struct(data) = data else {
        return Err(not_a_struct_err(span))
    };
    let fields = match &data.fields {
        Fields::Named(fields) => &fields.named,
        Fields::Unnamed(fields) => &fields.unnamed,
        Fields::Unit => { return Ok(VarianceKind::Static.unspanned()) }
    };
    let mut variance: Option<Variance> = None;
    for field in fields.iter() {
        search_for_variance(&field.attrs, &mut variance)?;
    }
    match variance {
        Some(variance) if !is_static => Ok(variance),
        Some(variance) => Err(static_type_with_variance(variance.span())),
        None if is_static => Ok(VarianceKind::Static.unspanned()),
        None => Ok(VarianceKind::Invariant.unspanned()),
    }
}

fn search_for_variance(
    attrs: &Vec<Attribute>, 
    variance: &mut Option<Variance>
) -> Result<()> {
    for attr in attrs.iter() {
        if attr.path().is_ident("variance") {
            attr.parse_nested_meta(|meta| {
                let ident = meta.path.get_ident().ok_or_else(|| 
                    Error::new(meta.path.span(), "Expected identifier!")
                )?;
                match variance.replace(Variance::from_ident(ident)?) {
                    Some(old) => {
                        let new = variance.as_ref().unwrap();
                        Err(duplicate_variance_err(&old, new))
                    },
                    None => Ok(())
                }
            })?;
        }
    };
    Ok(())
}

fn static_type_bound() -> TypeParamBound {parse_quote! { 'static }}
fn static_param() -> GenericParam {parse_quote! { 'static }}
fn no_generics() -> Generics {parse_quote! { <> }}


/// Struct storing AST nodes for the generic parameters in various forms.
struct Params {
    //                impl<'src, T> Transient for Struct<'src, ...> where
    impl_: Generics,    // <---'                             |        |
    original: Generics, // <---------------------------------'--------'
    //                type Static = Struct<'static, T>;
    static_: Generics,  // <----------------------'
    //                type Transience = Co<'src>;
    lifetime: Option<Lifetime>, // <--------'
}
impl Params {

    fn new(
        lifetime: Option<Lifetime>,
        original: Generics,
        impl_: Generics,
        static_: Vec<GenericParam>,
    ) -> Self {
        Params {lifetime, original, impl_,  static_: parse_quote!(<#(#static_,)*>)}
    }

    fn empty() -> Self {
        Params::new(None, no_generics(), no_generics(), vec![])
    }
    
    fn is_static(&self) -> bool {
        self.lifetime.is_none()
    }

    fn transience_generics(&self) -> Generics {
        match self.lifetime.as_ref() {
            Some(lifetime) => parse_quote!(<#lifetime>),
            None => no_generics()
        }
    }

    fn impl_generics(&self) -> &Generics {
        &self.impl_
    }

    fn split_for_impl(&self) -> (TypeGenerics, Option<&WhereClause>) {
        let (_, type_generics, where_clause) = self.original.split_for_impl();
        (type_generics, where_clause)
    }

    fn static_type_generics(&self) -> TypeGenerics {
        self.static_.split_for_impl().1
    }
}

fn process_param(param: &mut GenericParam) -> Result<()> {
    match param {
        GenericParam::Lifetime(lt) => Err(too_many_lifetimes_err(lt.span())),
        GenericParam::Type(ty) => {
            ty.bounds.push(static_type_bound());
            Ok(())
        },
        _ => Ok(())
    }
}

fn process_generics(generics: Generics) -> Result<Params> {
    // no generic params == ezpz
    if generics.params.is_empty() {
        return Ok(Params::empty())
    }
    // generics for impl<...> (same as orig, but with `'static` added to any type params)
    let mut impl_generics = generics.clone();
    let mut params_iter = impl_generics.params.iter_mut();

    // generics for the `Static` type (same as orig, but `'a` replaced by `'static`)
    let mut static_generics = vec![];

    // get lifetime from the first parameter
    let lifetime = match params_iter.next().unwrap() {
        GenericParam::Lifetime(lt) => {
            static_generics.push(static_param());
            Some(lt.lifetime.clone())
        },
        param_ => {
            static_generics.push(param_.clone());
            if let GenericParam::Type(ty) = param_ {
                ty.bounds.push(static_type_bound());
            }
            None
        }
    };
    // process remaining params
    for param in params_iter {
        static_generics.push(param.clone());
        process_param(param)?;
    }
    // collect params and return
    let params = Params::new(
        lifetime, generics, impl_generics,  static_generics,
    );
    Ok(params)
}


// === ERRORS === //

fn not_a_struct_err(span: Span) -> Error {
    Error::new(span, "Only `struct`'s are supported!")
}

fn too_many_lifetimes_err(span: Span) -> Error {
    Error::new(span, "At most one lifetime parameter is allowed!")
}

fn static_type_with_variance(span: Span) -> Error {
    Error::new(span, 
        "A variance cannot be requested for a struct without \n\
        a lifetime parameter!\n ")
}

fn duplicate_variance_err(old: &Variance, new: &Variance) -> Error {
    Error::new(new.span(),
        format!("Duplicate variance specification! '{old}' replaced by '{new}'\n "))
}

fn unexpected_variance_err(variance: &str, span: Span) -> Error {
    let msg = format!(
        "Unexpected variance argument '{}'! The valid options are: \n{:?}\n ", 
        variance, VALID_VARIANCES
    );
    Error::new(span, msg)
}

fn unsafe_variance_err(variance: &str, span: Span) -> Error {
    let msg = format!(
        "Setting the variance to '{variance}' is `unsafe`! Prefix the argument with \n\
        'unsafe_' ('#[variance(unsafe_{variance})]') after reviewing the safety docs \n\
        for the `transient::Transient` trait.\n ");
    Error::new(span, msg)
}
