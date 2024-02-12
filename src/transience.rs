use std::{fmt::Debug, marker::PhantomData};

/// Unsafe marker trait for types used to establish the [variance] for each
/// lifetime parameter of a struct.
///
/// Note that even though most types are *covariant* in reality, this crate
/// treats *invariance* as the default since any other assumption could cause
/// undefined behavior if chosen incorrectly. To override this default, the
/// [`Transience`] associated type can be set in the type's `Transient`
/// implementation; if using the derive macro, this corresponds to including
/// the `#[r#unsafe(covariant)]` attribute.
///
/// # SAFETY
/// Must be a zero-sized-type with the variance suggested by it's name.
///
/// [`Transience`]: crate::Transient::Transience
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
pub unsafe trait Transience: Debug {}// + IntoTransience<Self> {}

/// Specialized `Transience` for a single lifetime parameter.
///
/// Multi-lifetime `Transience` implementations are typically tuples containing
/// a single `Variance` for each lifetime.
///
/// # SAFETY
/// Must exhibit the variance suggested by it's name.
pub unsafe trait Variance<'a>: Transience {}


/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a *static* type with no generic lifetime parameters.
///
/// Such types only contain owned data and static references, and are thus much
/// safer to work with and allow several restrictions imposed by the crate to
/// be loosened.
pub type Timeless = ();
unsafe impl Transience for Timeless {}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *invariant* with respect to its lifetime parameter `'a`.
///
/// An *invariant* type is one for which the compiler cannot safely assume that
/// its lifetime may be shortened *or* lengthened (e.g. `'b` in `&'a mut &'b T`).
/// Such a type must therefore match the expected lifetime exactly when passed to
/// a function.
///
/// See the [`Transience`] documentation for more information.
#[derive(Clone, Copy, Debug, Default)]
pub struct Inv<'a>(PhantomData<fn(&'a ()) -> &'a ()>);
unsafe impl<'a> Transience for Inv<'a> {}
unsafe impl<'a> Variance<'a> for Inv<'a> {}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *covariant* with respect to its lifetime parameter `'a`.
///
/// A *covariant* type is one for which the compiler can safely *shorten* its
/// lifetime parameter as needed when passing it to a function; for example,
/// `&'a T` is *covariant* w.r.t. `'a`, so `&'static str` can be used where
/// `&'short str` is expected.
///
/// See the [`Transience`] documentation for more information.
#[derive(Clone, Copy, Debug)]
pub struct Co<'a>(PhantomData<&'a ()>);
unsafe impl<'a> Transience for Co<'a> {}
unsafe impl<'a> Variance<'a> for Co<'a> {}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *contravariant* with respect to its lifetime parameter `'a`.
///
/// A *contravariant* type is one for which the compiler can safely *lengthen*
/// its lifetime parameter as needed when passing it to a function; for example,
/// `fn(&'a str)` is *contravariant* w.r.t. `'a`, so `fn(&'short str)` can be
/// used where `fn(&'static str)` is expected.
///
/// See the [`Transience`] documentation for more information.
#[derive(Clone, Copy, Debug)]
pub struct Contra<'a>(PhantomData<fn(&'a ())>);
unsafe impl<'a> Transience for Contra<'a> {}
unsafe impl<'a> Variance<'a> for Contra<'a> {}


/// Marker trait indicating that the implementing [`Transience`] can safely
/// "downgrade" to the `Other` one.
///
/// A `SubTransience` can transcend to its `SuperTransience`. For example,
/// `Co<'long>` is a `SubTransience` of `Co<'short>`, and can thus transcend
/// to it freely.
///
/// The set of valid transitions (and implementers) can be summarized as:
/// - `Timeless` --> `R: Transience`
/// - `Inv<'a>` --> `Inv<'a>`
/// - `Co<'long>` --> `Co<'short>`
/// - `Contra<'short>` --> `Contra<'long>`
/// - `Contra<'short>` --> `Inv<'long>`
/// -
/// # SAFETY
/// This trait must only be implemented for *valid* conversions. Implementing
/// this trait for an *invalid* conversion (such as shortening the lifetime
/// of a *contravariant* type) can lead to undefined behavior.
pub unsafe trait CanTranscendTo<Super>: Transience + Sized {}

unsafe impl<R: Transience> CanTranscendTo<R> for Timeless {}
unsafe impl<'a> CanTranscendTo<Inv<'a>> for Inv<'a> {}
unsafe impl<'short, 'long: 'short> CanTranscendTo<Co<'short>> for Co<'long> {}
unsafe impl<'short, 'long: 'short> CanTranscendTo<Inv<'short>> for Co<'long> {}
unsafe impl<'short, 'long: 'short> CanTranscendTo<Contra<'long>> for Contra<'short> {}
unsafe impl<'short, 'long: 'short> CanTranscendTo<Inv<'long>> for Contra<'short> {}


/// Trait to recover as much as possible after transcending.
///
/// When a type is `Co<'long>`, it can transcend to `Inv<'short>`. We can't allow
/// fully reverting to `Co<'long>` because we erased the extra lifetime, but we
/// at least need to allow revert to `Co<'short>` or else a covariant type would
/// be lost forever once transcended to `Inv`.
///
/// The impls provided are very similar to `SubTransience`, except:
/// - `Inv` can recover to `Co` or `Contra` at the same lifetime
/// - Any transience can recover to `Timeless`
/// - `Timeless` can only recover to itself
pub unsafe trait CanRecoverFrom<From> {}

unsafe impl<V: Transience> CanRecoverFrom<V> for Timeless {}
unsafe impl<'a> CanRecoverFrom<Inv<'a>> for Inv<'a> {}
unsafe impl<'a> CanRecoverFrom<Inv<'a>> for Co<'a> {}
unsafe impl<'a> CanRecoverFrom<Inv<'a>> for Contra<'a> {}
unsafe impl<'short, 'long: 'short> CanRecoverFrom<Co<'long>> for Co<'short> {}
unsafe impl<'short, 'long: 'short> CanRecoverFrom<Co<'long>> for Inv<'short> {}
unsafe impl<'short, 'long: 'short> CanRecoverFrom<Contra<'short>> for Contra<'long> {}
unsafe impl<'short, 'long: 'short> CanRecoverFrom<Contra<'short>> for Inv<'long> {}


macro_rules! impl_transience_tuple {
    { ($($src:ident),*) => ($($dst:ident),*) } => {
        unsafe impl<$($src),*> Transience for ($($src),*,)
        where
            $( $src: Transience ),*
        {}
        unsafe impl<$($src),*, $($dst),*> CanTranscendTo<($($dst),*,)> for ($($src),*,)
        where
            $( $src: CanTranscendTo<$dst> ),* ,
            $( $dst: Transience ),* ,
        {}
        unsafe impl<$($src),*, $($dst),*> CanRecoverFrom<($($dst),*,)> for ($($src),*,)
        where
            $( $src: CanRecoverFrom<$dst> ),*
        {}
    }
}

impl_transience_tuple!{ (A1) => (A2) }
impl_transience_tuple!{ (A1, B1) => (A2, B2) }
impl_transience_tuple!{ (A1, B1, C1) => (A2, B2, C2) }
impl_transience_tuple!{ (A1, B1, C1, D1) => (A2, B2, C2, D2) }
