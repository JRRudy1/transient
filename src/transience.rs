//! Defines the `Transience` trait and implementations.
use std::marker::PhantomData;

/// Unsafe marker trait for types used to establish the [variance] for each
/// lifetime parameter of a struct.
///
/// Note that even though most types are *covariant* in reality, this crate
/// treats *invariance* as the default since any other assumption could cause
/// undefined behavior if chosen incorrectly. To override this default, the
/// [`Transience` associated type] can be set in the type's `Transient`
/// implementation; if using the derive macro, this corresponds to including
/// the `#[r#unsafe(covariant)]` attribute.
///
/// [`Transience` associated type]: crate::Transient::Transience
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
pub trait Transience: Sized + CanTranscendTo<Self> + CanRecoverFrom<Self> {}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a *static* type with no generic lifetime parameters.
///
/// Such types only contain owned data and static references, and are thus much
/// safer to work with and allow several restrictions imposed by the crate to
/// be loosened.
pub type Timeless = ();
impl Transience for Timeless {}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *invariant* with respect to its lifetime parameter `'a`.
///
/// An *invariant* type is one for which the compiler cannot safely assume that
/// its lifetime may be shortened *or* lengthened (e.g. `'b` in `&'a mut &'b T`).
/// Such a type must therefore match the expected lifetime exactly when passed to
/// a function.
///
/// See the [`Transience`] documentation for more information.
#[derive(Clone, Copy, Debug)]
pub struct Inv<'a>(PhantomData<fn(&'a ()) -> &'a ()>);
impl<'a> Transience for Inv<'a> {}

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
impl<'a> Transience for Co<'a> {}

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
impl<'a> Transience for Contra<'a> {}


/// Marker trait indicating that the implementing [`Transience`] can safely
/// upcast to `Other`.
///
/// A `SubTransience` can transcend to its `SuperTransience`. For example,
/// `Co<'long>` is a `SubTransience` of `Co<'short>`, and can thus transcend
/// to it freely.
///
/// The set of valid transitions can be summarized as:
/// - `Timeless` --> any `R: Transience`
/// - `Inv<'a>` --> `Inv<'a>`
/// - `Co<'long>` --> `Co<'short>`
/// - `Co<'long>` --> `Inv<'short>`
/// - `Contra<'short>` --> `Contra<'long>`
/// - `Contra<'short>` --> `Inv<'long>`
///
/// # SAFETY
/// This trait must only be implemented for *valid* conversions. Implementing
/// this trait for an *invalid* conversion (such as shortening the lifetime
/// of a *contravariant* type) can lead to undefined behavior.
pub unsafe trait CanTranscendTo<Other> {}


/// Marker trait indicating that the implementing [`Transience`] can be safely
/// recovered from the parameterizing `Transience`.
///
/// This is *almost* equivalent to the [`CanTranscendTo`] trait from the opposite
/// perspective, but with a few exceptions to allow more flexible recovery. For
/// example, we might erase a type `T: Transient<Transience=Co<'a>>` to
/// `dyn Any<Inv<'a>>` so that we can store it with invariant types, and this is
/// allowed because `Co<'a>: CanTranscendTo<Inv<'a>>`. However, downcasting the
/// erased type back to `T` would require reverting `Inv<'a>` to `Co<'a>`, which
/// is *not* a valid conversion according to the subtyping relationships modeled
/// by the `CanTranscendTo` trait. While it is true that converting `Inv<'a>` to
/// `Co<'a>` would not be sound in general, it is fine to do this while downcasting
/// because the trait abject is not kept around where the altered transience could
/// be abused and lead to UB.
///
/// The impls provided are very similar to `CanTranscendTo`, except:
/// - `Co` can be recovered from `Inv` at the same (or shorter) lifetime.
/// - `Contra` can be recovered from `Inv` at the same (or longer) lifetime.
/// - `Timeless` can be recovered from *any* transience.
/// - The only transience that can be recovered from `Timeless` is itself.
/// This is only for technical reasons (to avoid duplicate impls), not safety
/// considerations. In practice it doesn't sacrifice flexibility because the
/// only safe way to obtain `dyn Any<Timeless>` in the first place is if the
/// original type was `T: Transient<Transience=Timeless>`.
///
/// # SAFETY
/// This trait must only be implemented for *valid* conversions. Implementing
/// this trait for an *invalid* conversion (such as shortening the lifetime
/// of a *contravariant* type) can lead to undefined behavior.
pub unsafe trait CanRecoverFrom<From> {}


// ************************************************************************* //
// ************************* SAFETY-CRITICAL LOGIC! ************************ //
// ************************************************************************* //
// The following impls define the allowable transitions between variances,   //
// amd play key roles in upholding safety guarantees. All other impls are    //
// derived from these rules, so it is critical that they be correct.         //
// ************************************************************************* //

unsafe impl<R: Transience> CanTranscendTo<R> for Timeless {}
unsafe impl<R: Transience> CanRecoverFrom<R> for Timeless {}

unsafe impl<'a> CanTranscendTo<Inv<'a>> for Inv<'a> {}
unsafe impl<'a> CanRecoverFrom<Inv<'a>> for Inv<'a> {}

unsafe impl<'a, 'b: 'a> CanTranscendTo<Co<'a>> for Co<'b> {}
unsafe impl<'a, 'b: 'a> CanRecoverFrom<Co<'b>> for Co<'a> {}

unsafe impl<'a, 'b: 'a> CanTranscendTo<Inv<'a>> for Co<'b> {}
unsafe impl<'a, 'b: 'a> CanRecoverFrom<Co<'b>> for Inv<'a> {}

unsafe impl<'a, 'b: 'a> CanTranscendTo<Contra<'b>> for Contra<'a> {}
unsafe impl<'a, 'b: 'a> CanRecoverFrom<Contra<'a>> for Contra<'b> {}

unsafe impl<'a, 'b: 'a> CanTranscendTo<Inv<'b>> for Contra<'a> {}
unsafe impl<'a, 'b: 'a> CanRecoverFrom<Contra<'a>> for Inv<'b> {}

unsafe impl<'a> CanRecoverFrom<Inv<'a>> for Co<'a> {}
unsafe impl<'a> CanRecoverFrom<Inv<'a>> for Contra<'a> {}

// ************************************************************** //


/// Private macro implementing the transitions between each scalar 
/// transience and a 1-, 2-, or 3-tuple of compatible transiences. 
/// This is necessary because blanket impls would overlap.
macro_rules! impl_scalar_to_tuples {
    ( $($typ:ty),* ) => {
        $(
        // scalar => 1-tuple* => scalar
        unsafe impl<'a, R> CanTranscendTo<(R,)> for $typ
            where $typ: CanTranscendTo<R> {}
        unsafe impl<'a, R> CanRecoverFrom<(R,)> for $typ
            where $typ: CanRecoverFrom<R> {}

        // scalar => 2-tuple* => scalar
        unsafe impl<'a, R1, R2> CanTranscendTo<(R1, R2,)> for $typ
            where $typ: CanTranscendTo<R1> + CanTranscendTo<R2> {}
        unsafe impl<'a, R1, R2> CanRecoverFrom<(R1, R2,)> for $typ
            where $typ: CanRecoverFrom<R1> + CanRecoverFrom<R2> {}

        // scalar => 3-tuple* => scalar
        unsafe impl<'a, R1, R2, R3> CanTranscendTo<(R1, R2, R3,)> for $typ
            where $typ: CanTranscendTo<R1> + CanTranscendTo<R2> + CanTranscendTo<R3> {}
        unsafe impl<'a, R1, R2, R3> CanRecoverFrom<(R1, R2, R3,)> for $typ
            where $typ: CanRecoverFrom<R1> + CanRecoverFrom<R2> + CanRecoverFrom<R3> {}

        // ------------------------------------------

        // 1-tuple* => scalar => 1-tuple*
        unsafe impl<'a, R,> CanTranscendTo<$typ> for (R,)
            where R: CanTranscendTo<$typ> {}
        unsafe impl<'a, R,> CanRecoverFrom<$typ> for (R,)
            where R: CanRecoverFrom<$typ> {}
        /* !!! */

        // 2-tuple* => scalar => 2-tuple*
        unsafe impl<'a, R1, R2> CanTranscendTo<$typ> for (R1, R2)
            where R1: CanTranscendTo<$typ>, R2: CanTranscendTo<Self> {}
        unsafe impl<'a, R1, R2> CanRecoverFrom<$typ> for (R1, R2)
            where R1: CanRecoverFrom<$typ>, R2: CanRecoverFrom<Self> {}

        // 3-tuple* => scalar => 3-tuple*
        unsafe impl<'a, R1, R2, R3> CanTranscendTo<$typ> for (R1, R2, R3)
            where R1: CanTranscendTo<Self>,
                  R2: CanTranscendTo<Self>,
                  R3: CanTranscendTo<Self>,  {}
        unsafe impl<'a, R1, R2, R3> CanRecoverFrom<$typ> for (R1, R2, R3)
            where R1: CanRecoverFrom<Self>,
                  R2: CanRecoverFrom<Self>,
                  R3: CanRecoverFrom<Self>,  {}

        )*
    }
}
impl_scalar_to_tuples!{
    Co<'a>, Contra<'a>, Inv<'a>
}

/// implements transitions between equal-length tuples where each sub-transition is
/// also implemented (e.g., `(Co<'long>, Co<'short>)` -> `(Co<'short>, Inv<'short>)`)
macro_rules! impl_equal_tuples {
    { $( ($($src:ident,)*) => ($($dst:ident,)*) );* $(;)? } => {
        $(
        impl<$($src),*> Transience for ($($src),*,)
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
        )*
    }
}
impl_equal_tuples!{
    (A1,) => (A2,);
    (A1, B1,) => (A2, B2,);
    (A1, B1, C1,) => (A2, B2, C2,);
    (A1, B1, C1, D1,) => (A2, B2, C2, D2,);
}
