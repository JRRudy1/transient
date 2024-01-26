/*!
Defines the `TransientAny` derive macro that implements the `MakeStatic`
and `TransientAny` traits for a struct with at most 1 lifetime parameter.

While the `TransientAny` trait is not implemented directly by this macro, it
is implicitly implemented due to the blanked impl for all `T: MakeStatic`.
This macro is limited to structs satisfying the following conditions:

- There must be at most 1 lifetime parameter. Structs with extra lifetime
parameters can easily implement the trait by hand, but care must be taken
to ensure that its shortest lifetime is chosen for the implementation.

- There may be any number of type (or const) parameters, but the trait
will only be implemented where `T: 'static` for each type `T`.

Invocation with a type param and a lifetime:
```no_run
use transient_any::TransientAny;

#[derive(Debug, Clone, PartialEq, Eq, TransientAny)]
struct S<'a, T: 'static> {
    value: &'a T,
}
```
Generated impl:
```no_run
# pub mod transient_any {pub unsafe trait MakeStatic<'a> {type Static;}}
# struct S<'a, T> {value: &'a T}
unsafe impl<'a, T: 'static> transient_any::MakeStatic<'a> for S<'a, T> {
    type Static = S<'static, T>;
}
```
*/

use proc_macro::TokenStream;
use proc_macro2::{TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, Lifetime, DeriveInput,
    Generics, Result, GenericParam, TypeParamBound, Path,
    spanned::Spanned, TypeGenerics, WhereClause, Error,
};


/// Derive the `MakeStatic` trait.
#[proc_macro_derive(TransientAny)]
pub fn derive_make_static(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input as DeriveInput);
    let tokens = generate_impl(input)
        .unwrap_or_else(|e| e.to_compile_error());
    TokenStream::from(tokens)
}

fn trait_path() -> Path {
    parse_quote! { transient_any::MakeStatic }
}
fn static_type_bound() -> TypeParamBound {
    parse_quote! { 'static }
}
fn static_param() -> GenericParam {
    parse_quote! { 'static }
}
fn static_lifetime() -> Lifetime {
    parse_quote! { 'static }
}
fn no_generics() -> Generics {
    parse_quote! { <> }
}

/// Struct storing AST nodes for the generic parameters in various forms.
struct Params {
    //                impl<'src, T> MakeStatic<'src> for Struct<'src, ...> where
    impl_: Generics,    // <---'                 |                  |        |
    lifetime: Lifetime, // <---------------------'                  |        |
    original: Generics, // <----------------------------------------'--------'
    //                type Static = Struct<'static, T>;
    static_: Generics,  // <--------------------'
}
impl Params {

    fn new(
        lifetime: Lifetime,
        original: Generics,
        impl_: Generics,
        static_: Vec<GenericParam>
    ) -> Self {
        let static_: Generics = parse_quote!(<#(#static_,)*>);
        Params {lifetime, original, impl_,  static_}
    }

    fn empty() -> Self {
        Params {
            lifetime: static_lifetime(),
            original: no_generics(),
            impl_: no_generics(),
            static_: no_generics(),
        }
    }

    fn lifetime(&self) -> &Lifetime {
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

    // generics for the `Static` type (same as orig, but `'a` replaced by `'static`)
    let mut static_generics = vec![];

    // get lifetime from the first parameter
    let lifetime = match params_iter.next().unwrap() {
        GenericParam::Lifetime(lt) => {
            static_generics.push(static_param());
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
    Ok(Params::new(lifetime, generics, impl_generics, static_generics))
}


fn generate_impl(input: DeriveInput) -> Result<TokenStream2> {

    let name = input.ident;
    let trait_ = trait_path();

    let params = process_generics(input.generics)?;
    let lifetime = params.lifetime();
    let impl_generics = params.impl_generics();
    let (ty_generics, where_clause) = params.split_for_impl();
    let static_ty_generics = params.static_type_generics();

    let tokens = quote!(
        unsafe impl #impl_generics #trait_ <#lifetime> for #name #ty_generics
        #where_clause {
            type Static = #name #static_ty_generics;
        }
    );
    Ok(tokens)
}
