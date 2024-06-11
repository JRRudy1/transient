//! Defines the [`Transient`][`crate::Transient`] derive macro that implements
//! the `Transient` trait for a struct with at most 1 lifetime parameter.

#![allow(unused_imports)]
use std::mem;
use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::{quote, ToTokens};
use syn::{
    parse_macro_input, parse_quote, Lifetime, DeriveInput, Generics, 
    Result, GenericParam, TypeParamBound, Path, spanned::Spanned, 
    TypeGenerics, WhereClause, Error, Attribute, parse_quote_spanned,
    Meta, MetaList, Ident, meta::ParseNestedMeta, Type
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
/// # Examples
/// Invocation with a type param and a lifetime:
/// ```no_run
/// use transient::Transient;
///
/// #[derive(Debug, Clone, PartialEq, Eq, Transient)]
/// struct S<'a, T> {
///     value: &'a T,
/// }
/// ```
/// Generated impl:
/// ```
/// # struct S<'a, T> {value: &'a T}
/// unsafe impl<'a, T: 'static> transient::Transient for S<'a, T> {
///     type Static = S<'static, T>;
///     type Transience = transient::Inv<'a>;
/// }
/// ```
/// [`Transient`]: ../transient/trait.Transient.html
/// [safety docs]: ../transient/trait.Transient.html#[safety]
#[proc_macro_derive(Transient, attributes(r#unsafe, invariant, covariant, contravariant))]
pub fn derive_transient(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let tokens = generate_impl(input)
        .unwrap_or_else(|e| e.to_compile_error());
    TokenStream::from(tokens)
}

fn generate_impl(input: DeriveInput) -> Result<TokenStream2> {
    let name = input.ident.clone();
    let trait_ = trait_path();

    let params = process_generics(input.generics)?;
    let trait_generics = params.trait_generics();
    let impl_generics = params.impl_generics();
    let (ty_generics, where_clause) = params.split_for_impl();
    let static_ty_generics = params.static_type_generics();

    // future: requesting variance on a static struct should fail
    let variance_ty: Type = if params.is_static {
        let variance = Variance::Static.as_path();
        parse_quote!( #variance )
    } else {
        let variance = parse_attrs(&input.attrs)?.as_path();
        parse_quote!( #variance #trait_generics )
    };

    let tokens = quote!(
        unsafe impl #impl_generics #trait_ for #name #ty_generics
        #where_clause {
            type Static = #name #static_ty_generics;
            type Transience = #variance_ty;
        }
    );
    Ok(tokens)
}

const UNSAFE_MARKER: &'static str = "r#unsafe";
const INVARIANT: &'static str = "invariant";
const COVARIANT: &'static str = "covariant";
const CONTRAVARIANT: &'static str = "contravariant";


#[derive(Debug, Clone, PartialEq, Eq)]
enum Variance {
    Covariant,
    Contravariant,
    Invariant,
    Static,
}
impl Variance {
    fn as_path(&self) -> Path {
        match self {
            Variance::Invariant => parse_quote!(transient::Inv),
            Variance::Covariant => parse_quote!(transient::Co),
            Variance::Contravariant => parse_quote!(transient::Contra),
            Variance::Static => parse_quote!(transient::Timeless),
        }
    }
}

fn parse_attr(meta: &ParseNestedMeta, marked_unsafe: bool) -> Result<Option<Variance>> {
    let path = &meta.path;
    let span = path.span();
    let ident = path.get_ident().expect("expected ident").to_string();
    match (ident.to_lowercase().as_str(), marked_unsafe) {
        (INVARIANT, _) => Ok(Some(Variance::Invariant)),
        (COVARIANT, true) => Ok(Some(Variance::Covariant)),
        (CONTRAVARIANT, true) => Ok(Some(Variance::Contravariant)),
        (COVARIANT, false) => Err(unsafe_variance_err(COVARIANT, span)),
        (CONTRAVARIANT, false) => Err(unsafe_variance_err(CONTRAVARIANT, span)),
        _ => Err(unexpected_arg_err(path))
    }
}
fn parse_attrs(attrs: &Vec<Attribute>) -> Result<Variance> {
    let mut variance: Option<Variance> = None;
    for attr in attrs.iter() {
        let path = attr.path();
        if path.is_ident(UNSAFE_MARKER) {
            attr.parse_nested_meta(|meta| {
                let new = parse_attr(&meta, true)?;
                match (&variance, &new) {
                    (None, Some(_new)) => {variance = new; Ok(())},
                    (Some(_old), Some(_new)) if _old != _new => Err(
                        duplicate_variance_err(_old, _new, meta.path.span())),
                    _ => Ok(()),
                }
            })
        } else {
            Err(unexpected_attr_err(&path))
        }?;
    }
    Ok(variance.unwrap_or(Variance::Invariant))
}



fn trait_path() -> Path {parse_quote! {transient::Transient}}
fn static_type_bound() -> TypeParamBound {parse_quote! { 'static }}
fn static_param() -> GenericParam {parse_quote! { 'static }}
fn static_lifetime() -> Lifetime {parse_quote! { 'static }}
fn no_generics() -> Generics {parse_quote! { <> }}


/// Struct storing AST nodes for the generic parameters in various forms.
struct Params {
    //                impl<'src, T> MakeStatic<'src> for Struct<'src, ...> where
    impl_: Generics,    // <---'                 |                  |        |
    lifetime: Generics, // <---------------------'                  |        |
    original: Generics, // <----------------------------------------'--------'
    //                type Static = Struct<'static, T>;
    static_: Generics,  // <----------------------'
    is_static: bool,
}
impl Params {

    fn new(
        lifetime: Lifetime,
        original: Generics,
        impl_: Generics,
        static_: Vec<GenericParam>,
        is_static: bool,
    ) -> Self {
        let lifetime = parse_quote!(<#lifetime>);
        let static_: Generics = parse_quote!(<#(#static_,)*>);
        Params {lifetime, original, impl_,  static_, is_static}
    }

    fn empty() -> Self {
        Params::new(
            static_lifetime(),
            no_generics(),
            no_generics(),
            vec![],
            true,
        )
    }

    fn trait_generics(&self) -> &Generics {
        &self.lifetime
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
        GenericParam::Lifetime(lt) => Err(
            Error::new(lt.span(),
            "At most one lifetime parameter is allowed!"
        )),
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
    let mut is_static: bool = true;

    // generics for the `Static` type (same as orig, but `'a` replaced by `'static`)
    let mut static_generics = vec![];

    // get lifetime from the first parameter
    let lifetime = match params_iter.next().unwrap() {
        GenericParam::Lifetime(lt) => {
            static_generics.push(static_param());
            is_static = false;
            lt.lifetime.clone()
        },
        param_ => {
            static_generics.push(param_.clone());
            if let GenericParam::Type(ty) = param_ {
                ty.bounds.push(static_type_bound());
            }
            static_lifetime()
        }
    };
    // process remaining params
    for param in params_iter {
        static_generics.push(param.clone());
        process_param(param)?;
    }
    // collect params and return
    let params = Params::new(
        lifetime, generics, impl_generics,  static_generics, is_static);
    Ok(params)
}


// === ERRORS === //

fn duplicate_variance_err(old: &Variance, new: &Variance, span: Span) -> Error {
    let msg = format!(
        "Duplicate variance specification! {old:?} replaced with {new:?}");
    Error::new(span, msg)
}
fn unexpected_arg_err(path: &Path) -> Error {
    let ident = path.get_ident().expect("no ident");
    let msg = format!("Unexpected attribute {ident:?}");
    Error::new(path.span(), msg)
}
fn unexpected_attr_err(path: &Path) -> Error {
    let ident = path.get_ident().expect("no ident");
    let msg = format!("Unexpected attribute {ident:?}");
    Error::new(path.span(), msg)
}
fn unsafe_variance_err(variance: &'static str, span: Span) -> Error {
    let msg = format!(
        "Setting the variance to '{variance}' is unsafe! Wrap the argument \
        with `r#unsafe(...)` after reviewing the safety documentation!");
    Error::new(span, msg)
}
