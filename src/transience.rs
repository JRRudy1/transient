//! Defines the [`Transience`] trait as well as the [`Inv`], [`Co`], and [`Contra`]
//! structs that implement it. This module also defines the [`CanTranscendTo`] and
//! [`CanRecoverFrom`] traits that establish the allowable transitions between
//! transiences.
use std::marker::PhantomData;

/// Marker trait for types used to establish the [variance] of a type with
/// respect to each of its lifetime parameters, including [`Co`], [`Contra`],
/// [`Inv`], [`Timeless`], and tuples combining them.
///
/// Note that even though most types are *covariant* in reality, this crate
/// treats *invariance* as the default since any other assumption could cause
/// undefined behavior if chosen incorrectly. To override this default, the
/// [`Transience` associated type] can be set in the type's `Transient`
/// implementation; if using the derive macro, this corresponds to including
/// the `#[r#unsafe(covariant)]` attribute.
///
/// To maximize the flexibility of this crate's functionality, transitions
/// between compatible `Transiences` are supported. See the documentation for
/// the [`CanTranscendTo`] and[ `CanRecoverFrom`] traits for an introduction
/// to these transitions and when they are used, as well as a discussion of
/// why certain transitions are allowed while others are forbidden.
///
///
/// ## Valid transitions table
/// The following table summarizes the allowable transitions that can be
/// made between (single-lifetime) transiences, where the _rows_ represent
/// the starting point for a type (i.e. the `Transience` it defines in its
/// `Transient` impl), and the _columns_ represent a transience it wishes to
/// transition to (i.e. when being cast to `dyn Any<_>` or calling the
/// [`transcend`] method). Upon being [`downcast`] the trait object
/// would then return to a _row_ in the table, ideally back where it started
/// (although not always, as discussed in the next section):
///
/// <table style="width:80%">
///   <tr style="font-size: 12px;">
///     <th style="width:1%;"> </th>
///     <th> <code>Inv&lt'short&gt</code> </th>
///     <th> <code>Inv&lt'long&gt</code> </th>
///     <th> <code>Co&lt'short&gt</code> </th>
///     <th> <code>Co&lt'long&gt</code> </th>
///     <th> <code>Contra&lt'short&gt</code> </th>
///     <th> <code>Contra&lt'long&gt</code> </th>
///     <th> <code>Timeless</code> </th>
///   </tr>
///   <tr>
///     <th style="font-size: 12px;background-color:white"> <code>Inv&lt'short&gt</code> </th>
///     <td align="center" style="background-color:lightgrey;"> yes </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///   </tr>
///   <tr>
///     <th style="font-size: 12px;background-color:white"> <code>Inv&lt'long&gt</code> </th>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:lightgrey;"> yes </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///   </tr>
///   <tr>
///     <th style="font-size: 12px;background-color:white"> <code>Co&lt'short&gt</code> </th>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:lightgrey;"> yes </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///   </tr>
///   <tr>
///     <th style="font-size: 12px;background-color:white"> <code>Co&lt'long&gt</code> </th>
///     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
///     <td align="center" style="background-color:lightgrey;"> yes </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///   </tr>
///   <tr>
///     <th style="font-size: 12px;background-color:white"> <code>Contra&lt'short&gt</code> </th>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:lightgrey;"> yes </td>
///     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///   </tr>
///   <tr>
///     <th style="font-size: 12px;background-color:white"> <code>Contra&lt'long&gt</code> </th>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///     <td align="center" style="background-color:lightgrey;"> yes </td>
///     <td align="center" style="background-color:#FF9B9B;"> no </td>
///   </tr>
///   <tr>
///     <th style="font-size: 12px;background-color:white"> <code>Timeless</code> </th>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:lightgreen;"> yes </td>
///     <td align="center" style="background-color:lightgrey;"> yes </td>
///   </tr>
/// </table>
///
/// ## Lossy transitions
/// The yellow transitions marked with an asterisk in the above table are allowed,
/// but lossy; the recovered lifetime will be reduced (`Co`) or increased (`Contra`).
/// For example, a type `&'long i32` with a transience of `Co<'long>` (middle row)
/// can be safely erased to the trait object `dyn Any<Co<'short>>` (3rd column) by
/// shortening its lifetime. However, when we want to
/// [`downcast`] the opaque trait object back into its concrete type, we _cannot_ just
/// assume (at least under the implementation of this crate) that we can safely recover
/// a `'long` lifetime from the `'short` lifetime of the trait object;
/// instead, we can only safely recover `&'short i32` from the `dyn Any<Co<'short>>`.
/// As a result, even though the underlying reference should actually be valid for the
/// `'long` lifetime, we end up with a reference that the borrow-checker only believes
/// to be valid for the `'short` lifetime. This may seem silly if you happen to be
/// dealing with a case where _you know_ what you are working with and that the
/// `'long` lifetime _is_ valid; but the whole point of type-erasure is that
/// sometimes you _dont_ know/care, or you don't want to compiler to know/care,
/// and sometimes ignorance has a cost. On the bright side, if you _really
/// are_ confident that `'long` is valid, you can `unsafe`-ly fix it using either
/// the [`transcend_unbounded`] method or conventional hacks like
/// [`std::mem::transmute`] and raw pointer casts (at your own risk, of course).
///
/// [`Transience` associated type]: crate::Transient::Transience
/// [`Transcend::transcend`]: crate::Transcend::transcend
/// [`transcend`]: crate::Transcend::transcend
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
/// [`downcast`]: crate::Downcast::downcast
/// [`transcend_unbounded`]: crate::Transcend::transcend_unbounded
pub trait Transience: Sized + CanTranscendTo<Self> + CanRecoverFrom<Self> {}

/// Unsafe marker trait indicating that the implementing [`Transience`] can
/// safely upcast to the parameterizing `Transience`.
///
/// When `R: Transience` implements `CanTranscendTo<Other>`, it is making an
/// `unsafe` assertion that it is a [subtype] of `Other` and can safely be used
/// anywhere  that `Other` is expected. In the context of the `transient` crate,
/// this implementation empowers a type [`T: Transient<Transience=R>`] to be
/// erased to [`dyn Any<Other>`] in addition the `dyn Any<R>` suggested by
/// its `Transient` impl. Additionally, the existence of this impl allows the
/// [`Transcend::transcend::<Other>`] method to be called to convert `dyn Any<R>`
/// to `dyn Any<Other>`. Either of these operations would result in undefined
/// behavior if `R` was not actually a subtype of `Other`, hence the `unsafe`
/// declaration on this trait.
///
/// This trait is closely related to the [`CanRecoverFrom`] trait which bounds
/// the transiences that can be recovered when restored `dyn Any<R>` to its
/// concrete type `T`.
///
/// The foundational set of valid transitions (which correspond to implementations
/// of this trait) can be summarized as:
///
/// - `Timeless` --> any `R: Transience`
/// - `Co<'long>` --> `Co<'short>`
/// - `Co<'long>` --> `Inv<'short>`
/// - `Contra<'short>` --> `Contra<'long>`
/// - `Contra<'short>` --> `Inv<'long>`
/// - any `R: Transience` --> Self
///
/// Additionally, this trait is _composable_ such that a tuple of transiences
/// can safely implement it for another equal-length tuple when each of its
/// components implement the trait for the corresponding component in the
/// target tuple (i.e. `(R1, R2): CanTranscendTo<(S1, S2)>` iff
/// `R1: CanTranscendTo<S1>` and `R2: CanTranscendTo<S2>`. These transitions
/// are not exhaustively implemented, but can be expanded as needs arise.
///
/// Finally, some convenience implementations such as `R` --> `(R,)` and
/// `(R,)` --> `R` are provided.
///
/// # Safety
/// This trait must only be implemented for *valid* conversions in accordance
/// with [subtype] relationships as discussed above. Implementing this trait
/// for an *invalid* conversion (such as shortening the lifetime of a
/// *contravariant* type) can lead to undefined behavior.
///
/// [`T: Transient<Transience=R>`]: crate::Transient
/// [`dyn Any<Other>`]: crate::Any
/// [`Transcend::transcend::<Other>`]: crate::Transcend::transcend
/// [subtype]: https://doc.rust-lang.org/nomicon/subtyping.html
pub unsafe trait CanTranscendTo<Other> {}

/// Unsafe marker trait indicating that the implementing [`Transience`] can be
/// safely recovered from the parameterizing `Transience`.
///
/// When `R: Transience` implements `CanRecoverFrom<Other>`, it empowers a type
/// [`T: Transient<Transience=R>`] to be "recovered from" [`dyn Any<Other>`]
/// when downcasting using the [`Downcast::downcast::<R>`] and similar methods.
/// Allowing this operation when not appropriate, such as allowing `&'long i32`
/// (which implements `Transient<Transience=Co<'long>>`) to be recovered from
/// a `dyn Any<Co<'short>>` which may have started as `&'short i32`, could
/// easily lead to undefined behavior.
///
/// This trait is *almost* equivalent to the [`CanTranscendTo`] trait from the
/// opposite perspective, but with a few exceptions to allow more flexible
/// recovery. For example, we might erase a type `T: Transient<Transience=Co<'a>>`
/// to `dyn Any<Inv<'a>>` so that we can store it with invariant types, and this is
/// allowed because `Co<'a>: CanTranscendTo<Inv<'a>>` (i.e. `Co<'a>` is a [subtype]
/// of `Inv<'a>`) . However, downcasting the erased type back to `T` would require
/// reverting `Inv<'a>` to `Co<'a>`, which is *not* a valid conversion according
/// to the subtyping relationships as modeled by the `CanTranscendTo` trait. While
/// it is true that converting `Inv<'a>` to `Co<'a>` would not be sound in general,
/// it is fine to do this while downcasting because the trait abject is not kept
/// around where the altered transience could be abused and lead to UB.
///
/// The impls provided are very similar to `CanTranscendTo`, except:
/// - `Co` can be recovered from `Inv` at the same (or shorter) lifetime.
/// - `Contra` can be recovered from `Inv` at the same (or longer) lifetime.
/// - `Timeless` can be recovered from *any* transience.
/// - The only transience that can be recovered from `Timeless` is itself. This
/// is only for technical reasons (to avoid duplicate impls), and in practice it
/// doesn't sacrifice flexibility because the only safe way to obtain `dyn Any<())>`
/// in the first place is if the original type was `T: Transient<Transience=()>`.
///
/// # Safety
/// This trait must only be implemented for *valid* conversions. Implementing
/// this trait for an *invalid* conversion (such as shortening the lifetime
/// of a *contravariant* type) can lead to undefined behavior.
///
/// [`Downcast::downcast::<R>`]: crate::Downcast::downcast
/// [subtype]: https://doc.rust-lang.org/nomicon/subtyping.html
pub unsafe trait CanRecoverFrom<From> {}

/// Used as the `Transience` of a type to declare that it is `'static` and not
/// dependent on any lifetime parameters.
///
/// Such types only contain owned data and static references, and are thus much
/// safer to work with and allow several restrictions imposed by the crate to
/// be loosened. The [`transient::Any`][crate::Any] trait is parameterized with
/// this transience by default so that it can mimic the simplicity of the
/// [`std::any::Any`] trait in the simple case of `'static` types.
pub type Timeless = ();
impl Transience for Timeless {}

/// Used to declare an [_invariant_] relationship between a type and its lifetime
/// parameter.
///
/// An *invariant* type is one for which the compiler cannot safely assume that
/// its lifetime may be shortened *or* lengthened (e.g. `'b` in `&'a mut &'b T`).
/// Such a type must therefore match the expected lifetime exactly when passed to
/// a function.
///
/// See the [`Transience`] documentation for more information.
///
/// [_invariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
#[derive(Clone, Copy, Debug)]
pub struct Inv<'a>(PhantomData<fn(&'a ()) -> &'a ()>);

impl<'a> Transience for Inv<'a> {}

/// Used to declare an [_covariant_] relationship between a type and its lifetime
/// parameter.
///
/// A *covariant* type is one for which the compiler can safely *shorten* its
/// lifetime parameter as needed when passing it to a function; for example,
/// `&'a T` is *covariant* w.r.t. `'a`, so `&'static str` can be used where
/// `&'short str` is expected.
///
/// See the [`Transience`] documentation for more information.
///
/// [_covariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
#[derive(Clone, Copy, Debug)]
pub struct Co<'a>(PhantomData<&'a ()>);

impl<'a> Transience for Co<'a> {}

/// Used to declare an [_contravariant_] relationship between a type and its lifetime
/// parameter.
///
/// A *contravariant* type is one for which the compiler can safely *lengthen*
/// its lifetime parameter as needed when passing it to a function; for example,
/// `fn(&'a str)` is *contravariant* w.r.t. `'a`, so `fn(&'short str)` can be
/// used where `fn(&'static str)` is expected.
///
/// See the [`Transience`] documentation for more information.
///
/// [_contravariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
#[derive(Clone, Copy, Debug)]
pub struct Contra<'a>(PhantomData<fn(&'a ())>);

impl<'a> Transience for Contra<'a> {}

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

        // scalar => 4-tuple* => scalar
        unsafe impl<'a, R1, R2, R3, R4> CanTranscendTo<(R1, R2, R3, R4)> for $typ
            where $typ: CanTranscendTo<R1> + CanTranscendTo<R2>
                      + CanTranscendTo<R3> + CanTranscendTo<R4> {}
        unsafe impl<'a, R1, R2, R3, R4> CanRecoverFrom<(R1, R2, R3, R4)> for $typ
            where $typ: CanRecoverFrom<R1> + CanRecoverFrom<R2>
                      + CanRecoverFrom<R3> + CanRecoverFrom<R4> {}

        // ------------------------------------------

        // 1-tuple* => scalar => 1-tuple*
        unsafe impl<'a, R,> CanTranscendTo<$typ> for (R,)
            where R: CanTranscendTo<$typ> {}
        unsafe impl<'a, R,> CanRecoverFrom<$typ> for (R,)
            where R: CanRecoverFrom<$typ> {}

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

        // 4-tuple* => scalar => 4-tuple*
        unsafe impl<'a, R1, R2, R3, R4> CanTranscendTo<$typ> for (R1, R2, R3, R4)
            where R1: CanTranscendTo<Self>,
                  R2: CanTranscendTo<Self>,
                  R3: CanTranscendTo<Self>,
                  R4: CanTranscendTo<Self>,  {}
        unsafe impl<'a, R1, R2, R3, R4> CanRecoverFrom<$typ> for (R1, R2, R3, R4)
            where R1: CanRecoverFrom<Self>,
                  R2: CanRecoverFrom<Self>,
                  R3: CanRecoverFrom<Self>,
                  R4: CanRecoverFrom<Self>,  {}

        )*
    }
}
impl_scalar_to_tuples! {
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
impl_equal_tuples! {
    (A1,) => (A2,);
    (A1, B1,) => (A2, B2,);
    (A1, B1, C1,) => (A2, B2, C2,);
    (A1, B1, C1, D1,) => (A2, B2, C2, D2,);
}
