//! Defines the [`Transience`] trait as well as the [`Inv`], [`Co`], and [`Contra`]
//! structs that implement it. This module also defines the [`CanTranscendTo`] and
//! [`CanRecoverFrom`] traits that establish the allowable transitions between
//! transiences.
use crate::tr::Transient;
use core::marker::PhantomData;

/// Marker trait for types used to establish the [variance] of a type with
/// respect to each of its lifetime parameters, including [`Co`], [`Contra`],
/// [`Inv`], [`Timeless`], and tuples combining them.
///
/// Note that even though most types are *covariant* in reality, this crate
/// treats *invariance* as the default since any other assumption could cause
/// undefined behavior if chosen incorrectly. To override this default, the
/// [`Transience` associated type] can be set in the type's `Transient`
/// implementation; if using the derive macro, this corresponds to including
/// the `#[covariant]` or `#[contravariant]` attribute.
///
/// To maximize the flexibility of this crate's functionality, transitions
/// between compatible `Transiences` are supported. See the documentation for
/// the [`CanTranscendTo`] and[ `CanRecoverFrom`] traits for an introduction
/// to these transitions and when they are used, as well as a discussion of
/// why certain transitions are allowed while others are forbidden.
///
/// ## Valid transitions table
/// The following table summarizes the allowable transitions that can be
/// made between (single-lifetime) transiences, where the _rows_ represent
/// the starting point for a type (i.e. the `Transience` it defines in its
/// `Transient` impl), and the _columns_ represent a transience it wishes to
/// transition to (i.e. when being cast to `dyn Any<_>`). Upon being
/// [`downcast`] the trait object would then return to a _row_ in the table,
/// ideally back where it started (although not always, as discussed in the
/// next section):
///
/// <table style="width:80%">
///   <tr style="font-size: 12px;background-color:white">
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
/// [`downcast`] the opaque trait object back into its concrete type, we _cannot_
/// assume that we can safely recover
/// a `'long` lifetime from the `'short` lifetime of the trait object. The only truly
/// safe solution is to avoid downcasting to a shorter lifetime - just as if you were
/// working with references to the type itself.
///
/// # Safety
/// The `Inverse` associated types must be defined appropriately to reflect the
/// _opposite_ variance behavior with respect to the same lifetime parameter(s).
/// Similarly, the `Invariant` associated type must be defined as an _invariant_
/// behavior with respect to the same lifetime parameter(s). Note the this does
/// _not_ apply to the `Timeless` type, which has no lifetime relationships and
/// may thus use `Timeless` for both associated types.
///
/// [`Transience` associated type]: Transient::Transience
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
/// [`downcast`]: crate::Downcast::downcast
pub unsafe trait Transience:
    Transient + CanTranscendTo<Self> + CanRecoverFrom<Self>
{
    /// The inverse of this transience. `Co` <=> `Contra`, and `Inv` <=> `Inv`.
    type Inverse: Transience;
    /// The invariant version of this transience. `Co`, `Contra` => `Inv`.
    type Invariant: Transience;
}

/// Unsafe marker trait indicating that the implementing [`Transience`] can
/// safely upcast to the parameterizing `Transience`.
///
/// When `R: Transience` implements `CanTranscendTo<Other>`, it is making an
/// `unsafe` assertion that it is a [subtype] of `Other` and can safely be used
/// anywhere  that `Other` is expected. In the context of the `transient` crate,
/// this implementation empowers a type [`T: Transient<Transience=R>`] to be
/// erased to [`dyn Any<Other>`] in addition the `dyn Any<R>` suggested by its
/// `Transient` impl. This would result in undefined behavior if `R` was not
/// actually a subtype of `Other`, hence the `unsafe` declaration on this trait.
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
///   is only for technical reasons (to avoid duplicate impls), and in practice it
///   doesn't sacrifice flexibility because the only safe way to obtain `dyn Any<())>`
///   in the first place is if the original type was `T: Transient<Transience=()>`.
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

unsafe impl Transience for Timeless {
    type Inverse = ();
    type Invariant = ();
}

/// Used to declare an [_invariant_] relationship between a type and its lifetime
/// parameter.
///
/// An [_invariant_] type is one for which the compiler cannot safely assume that
/// its lifetime may be shortened *or* lengthened (e.g. `'b` in `&'a mut &'b T`).
/// Such a type must therefore match the expected lifetime exactly when passed to
/// a function.
///
/// See the [`Transience`] documentation for more information.
///
/// [_invariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
#[derive(Clone, Copy, Debug)]
pub struct Inv<'a>(PhantomData<fn(&'a ()) -> &'a ()>);

unsafe impl<'a> Transience for Inv<'a> {
    type Inverse = Inv<'a>;
    type Invariant = Inv<'a>;
}

unsafe impl<'a> Transient for Inv<'a> {
    type Static = Inv<'static>;
    type Transience = Self;
}

/// Used to declare a [_covariant_] relationship between a type and its lifetime
/// parameter.
///
/// A [_covariant_] type is one for which the compiler can safely *shorten* its
/// lifetime parameter as needed when passing it to a function; for example,
/// `&'a T` is *covariant* w.r.t. `'a`, so `&'long str` can be used where
/// `&'short str` is expected.
///
/// See the [`Transience`] documentation for more information.
///
/// [_covariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
#[derive(Clone, Copy, Debug)]
pub struct Co<'a>(PhantomData<&'a ()>);

unsafe impl<'a> Transience for Co<'a> {
    type Inverse = Contra<'a>;
    type Invariant = Inv<'a>;
}

unsafe impl<'a> Transient for Co<'a> {
    type Static = Co<'static>;
    type Transience = Self;
}

/// Used to declare a [_contravariant_] relationship between a type and its lifetime
/// parameter.
///
/// A [_contravariant_] type is one for which the compiler can safely *lengthen*
/// its lifetime parameter as needed when passing it to a function; for example,
/// `fn(&'a str)` is *contravariant* w.r.t. `'a`, so `fn(&'short str)` can be
/// used where `fn(&'long str)` is expected.
///
/// See the [`Transience`] documentation for more information.
///
/// [_contravariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
#[derive(Clone, Copy, Debug)]
pub struct Contra<'a>(PhantomData<fn(&'a ())>);

unsafe impl<'a> Transience for Contra<'a> {
    type Inverse = Co<'a>;
    type Invariant = Inv<'a>;
}

unsafe impl<'a> Transient for Contra<'a> {
    type Static = Contra<'static>;
    type Transience = Self;
}

/// Type alias that can be used in the [`Transience`] of a type to declare that it is
/// [_covariant_] with respect to a [`Transient`] type parameter.
///
/// This is analogous to the [`Co<'a>`][Co] type that is used to declare a covariant
/// relationship with respect to a lifetime parameter.
///
/// As discussed in its safety docs, the [`Transient`] trait requires an implementing
/// type to declare its temporal relationships in the [`Transience` associated type].
/// This most naturally refers to its variance with respect to its _lifetime_ parameters,
/// but it is _critical_ to account for that of its _type_ parameters as well when they
/// are allowed to be non-`'static`. This type, as well as the related [`Invariant`] and
/// [`Contravariant`] types, may be used to declare the variance of this  relationship
/// by using it, or a tuple including it, as the `Transience`.
///
/// # Example
/// Consider the following type, which is _covariant_ with respect to both the
/// lifetime parameter `'a` and the type parameter `T`:
/// ```
/// struct S<'a, T>(&'a [T]);
/// ```
///
/// If we wanted to provide a `Transient` implementation for this type that allows
/// the type parameter to be non-`'static`, we can use this `Covariant<T>` type
/// alias as a component in the `Transience` associated type:
/// ```
/// # struct S<'a, T>(&'a [T]);
/// use transient::{Transient, Co, Covariant};
///
/// unsafe impl<'a, T> Transient for S<'a, T>
/// where
///     T: Transient
/// {
///     type Static = S<'static, T::Static>;
///     type Transience = (Co<'a>, Covariant<T>);
/// }
/// ```
///
/// When a concrete type of `&'b i32` is substituted for `T`, this impl expands to
/// the following which properly accounts for the lifetime `'a`, as well as the
/// lifetime `'b` that was hidden inside of the generic parameter `T`:
/// ```
/// # struct S<'a, T>(&'a [T]);
/// # use transient::{Transient, Co, Covariant};
/// unsafe impl<'a, 'b> Transient for S<'a, &'b i32> {
///     type Static = S<'static, &'static i32>;
///     type Transience = (Co<'a>, Co<'b>);
/// }
/// ```
///
/// [`Static` associated type]: Transient::Transience
/// [`Transience` associated type]: Transient::Transience
/// [_covariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
pub type Covariant<T> = <T as Transient>::Transience;

/// Type alias that can be used in the [`Transience`] of a type to declare that it is
/// [_contravariant_] with respect to a [`Transient`] type parameter.
///
/// This is analogous to the [`Contra<'a>`][Contra] type that is used to declare a
/// contravariant relationship with respect to a lifetime parameter.
///
/// See the [`Covariant`] type for more information, including a usage example.
///
/// [_contravariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
pub type Contravariant<T> = <<T as Transient>::Transience as Transience>::Inverse;

/// Type alias that can be used in the [`Transience`] of a type to declare that it is
/// [_invariant_] with respect to a [`Transient`] type parameter.
///
/// This is analogous to the [`Inv<'a>`][Inv] type that is used to declare an invariant
/// relationship with respect to a lifetime parameter.
///
/// See the [`Covariant`] type for more information, including a usage example.
///
/// [_invariant_]: https://doc.rust-lang.org/nomicon/subtyping.html
pub type Invariant<T> = <<T as Transient>::Transience as Transience>::Invariant;

/// Type alias that can be nested within the [`Covariant`], [`Contravariant`], or
/// [`Invariant`] type to declare the [_variance_] of a [`Transient`] struct with
/// respect to a lifetime parameter.
///
/// Using this type is purely for aesthetics, and reduces to one of the fundamental
/// [`Transience`] types:
///
/// - `Covariant<Lifetime<'a>>` is equivalent to [`Co<'a>`]
/// - `Contravariant<Lifetime<'a>>` is equivalent to [`Contra<'a>`]
/// - `Invariant<Lifetime<'a>>` is equivalent to [`Inv<'a>`]
///
/// Using `Co`, `Contra`, or `Inv` directly should usually be preferred, but some users
/// may find that this type increases clarity when use alongside type parameter variances
/// such as in the `Transience` tuple `(Covariant<Lifetime<'a>>, Covariant<T>>)`
///
/// [_variance_]: https://doc.rust-lang.org/nomicon/subtyping.html
pub type Lifetime<'a> = PhantomData<&'a ()>;

// ************************************************************************* //
// ************************* SAFETY-CRITICAL LOGIC! ************************ //
// ************************************************************************* //
// The following impls define the allowable transitions between variances,   //
// amd play key roles in upholding safety guarantees. All other impls are    //
// derived from these rules, so it is critical that they be correct.         //
// ************************************************************************* //

// This has to be individual impls, to avoid conflicting with the blanket
// impls below
unsafe impl CanTranscendTo<Timeless> for Timeless {}
unsafe impl CanRecoverFrom<Timeless> for Timeless {}
unsafe impl<'a> CanTranscendTo<Co<'a>> for Timeless {}
unsafe impl<'a> CanRecoverFrom<Co<'a>> for Timeless {}
unsafe impl<'a> CanTranscendTo<Contra<'a>> for Timeless {}
unsafe impl<'a> CanRecoverFrom<Contra<'a>> for Timeless {}
unsafe impl<'a> CanTranscendTo<Inv<'a>> for Timeless {}
unsafe impl<'a> CanRecoverFrom<Inv<'a>> for Timeless {}

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

        // scalar => 5-tuple* => scalar
        unsafe impl<'a, R1, R2, R3, R4, R5> CanTranscendTo<(R1, R2, R3, R4, R5)> for $typ
            where $typ: CanTranscendTo<R1> + CanTranscendTo<R2>
                      + CanTranscendTo<R3> + CanTranscendTo<R4>
                      + CanTranscendTo<R5> {}
        unsafe impl<'a, R1, R2, R3, R4, R5> CanRecoverFrom<(R1, R2, R3, R4, R5)> for $typ
            where $typ: CanRecoverFrom<R1> + CanRecoverFrom<R2>
                      + CanRecoverFrom<R3> + CanRecoverFrom<R4>
                      + CanRecoverFrom<R5>{}

        // ------------------------------------------

        // 1-tuple* => scalar => 1-tuple*
        unsafe impl<'a, R,> CanTranscendTo<$typ> for (R,)
            where R: CanTranscendTo<$typ> {}
        unsafe impl<'a, R,> CanRecoverFrom<$typ> for (R,)
            where R: CanRecoverFrom<$typ> {}

        // 2-tuple* => scalar => 2-tuple*
        unsafe impl<'a, R1, R2> CanTranscendTo<$typ> for (R1, R2)
            where R1: CanTranscendTo<$typ>, R2: CanTranscendTo<$typ> {}
        unsafe impl<'a, R1, R2> CanRecoverFrom<$typ> for (R1, R2)
            where R1: CanRecoverFrom<$typ>, R2: CanRecoverFrom<$typ> {}

        // 3-tuple* => scalar => 3-tuple*
        unsafe impl<'a, R1, R2, R3> CanTranscendTo<$typ> for (R1, R2, R3)
            where R1: CanTranscendTo<$typ>,
                  R2: CanTranscendTo<$typ>,
                  R3: CanTranscendTo<$typ>,  {}
        unsafe impl<'a, R1, R2, R3> CanRecoverFrom<$typ> for (R1, R2, R3)
            where R1: CanRecoverFrom<$typ>,
                  R2: CanRecoverFrom<$typ>,
                  R3: CanRecoverFrom<$typ>,  {}

        // 4-tuple* => scalar => 4-tuple*
        unsafe impl<'a, R1, R2, R3, R4> CanTranscendTo<$typ> for (R1, R2, R3, R4)
            where R1: CanTranscendTo<$typ>,
                  R2: CanTranscendTo<$typ>,
                  R3: CanTranscendTo<$typ>,
                  R4: CanTranscendTo<$typ>,  {}
        unsafe impl<'a, R1, R2, R3, R4> CanRecoverFrom<$typ> for (R1, R2, R3, R4)
            where R1: CanRecoverFrom<$typ>,
                  R2: CanRecoverFrom<$typ>,
                  R3: CanRecoverFrom<$typ>,
                  R4: CanRecoverFrom<$typ>,  {}

        // 5-tuple* => scalar => 5-tuple*
        unsafe impl<'a, R1, R2, R3, R4, R5> CanTranscendTo<$typ> for (R1, R2, R3, R4, R5)
            where R1: CanTranscendTo<$typ>,
                  R2: CanTranscendTo<$typ>,
                  R3: CanTranscendTo<$typ>,
                  R4: CanTranscendTo<$typ>,
                  R5: CanTranscendTo<$typ>,  {}
        unsafe impl<'a, R1, R2, R3, R4, R5> CanRecoverFrom<$typ> for (R1, R2, R3, R4, R5)
            where R1: CanRecoverFrom<$typ>,
                  R2: CanRecoverFrom<$typ>,
                  R3: CanRecoverFrom<$typ>,
                  R4: CanRecoverFrom<$typ>,
                  R5: CanRecoverFrom<$typ>,  {}
        )*
    }
}
impl_scalar_to_tuples! {
    Co<'a>, Contra<'a>, Inv<'a>, Timeless
}

/// implements transitions between equal-length tuples where each sub-transition is
/// also implemented (e.g., `(Co<'long>, Co<'short>)` -> `(Co<'short>, Inv<'short>)`)
macro_rules! impl_equal_tuples {
    { $( ($($src:ident,)*) => ($($dst:ident,)*) );* $(;)? } => {
        $(
        unsafe impl<$($src),*> Transience for ($($src),*,)
        where
            $( $src: Transience ),*
        {
            type Inverse = ($(<$src as Transience>::Inverse),*,);
            type Invariant = ($(<$src as Transience>::Invariant),*,);
        }
        unsafe impl<$($src),*> Transient for ($($src),*,)
        where
            $( $src: Transient ),*
        {
            type Static = ($(<$src as Transient>::Static),*,);
            type Transience = ($(<$src as Transient>::Transience),*,);
        }
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
    (A1, B1, C1, D1, E1,) => (A2, B2, C2, D2, E2,);
}

#[cfg(test)]
mod tests {
    use crate::Static;

    use super::*;

    fn assert_can_transend_to<T, Tr>()
    where
        T: Transient,
        T::Transience: CanTranscendTo<Tr>,
    {
    }

    #[test]
    fn validate_static() {
        struct A;
        impl Static for A {}
        struct B;
        impl Static for B {}
        assert_can_transend_to::<(A, B), ()>();
    }

    #[test]
    fn validate_co<'a>() {
        #[allow(dead_code)]
        struct A<'a>(&'a ());
        unsafe impl<'a> Transient for A<'a> {
            type Static = A<'static>;
            type Transience = Co<'a>;
        }
        struct B;
        impl Static for B {}
        assert_can_transend_to::<(A, B), Co<'a>>();
        #[allow(dead_code)]
        struct C<'a>(&'a ());
        unsafe impl<'a> Transient for C<'a> {
            type Static = C<'static>;
            type Transience = Co<'a>;
        }
        assert_can_transend_to::<(A<'a>, C), Co<'a>>();
    }

    #[test]
    fn validate_contra<'a>() {
        #[allow(dead_code)]
        struct A<'a>(fn(&'a ()));
        unsafe impl<'a> Transient for A<'a> {
            type Static = A<'static>;
            type Transience = Contra<'a>;
        }
        struct B;
        impl Static for B {}
        assert_can_transend_to::<(A<'a>, B), Contra<'a>>();
        #[allow(dead_code)]
        struct C<'a>(fn(&'a ()));
        unsafe impl<'a> Transient for C<'a> {
            type Static = C<'static>;
            type Transience = Contra<'a>;
        }
        assert_can_transend_to::<(A<'a>, C<'a>), Contra<'a>>();
    }

    #[test]
    fn validate_inv<'a>() {
        #[allow(dead_code)]
        struct A<'a>(fn(&'a ()) -> &'a ());
        unsafe impl<'a> Transient for A<'a> {
            type Static = A<'static>;
            type Transience = Inv<'a>;
        }
        struct B;
        impl Static for B {}
        assert_can_transend_to::<(A<'a>, B), Inv<'a>>();
        #[allow(dead_code)]
        struct C<'a>(fn(&'a ()) -> &'a ());
        unsafe impl<'a> Transient for C<'a> {
            type Static = C<'static>;
            type Transience = Inv<'a>;
        }
        assert_can_transend_to::<(A<'a>, C<'a>), Inv<'a>>();
    }

    #[test]
    fn validate_mixed<'a>() {
        #[allow(dead_code)]
        struct A<'a>(fn(&'a ()));
        unsafe impl<'a> Transient for A<'a> {
            type Static = A<'static>;
            type Transience = Co<'a>;
        }
        #[allow(dead_code)]
        struct B<'a>(fn(&'a ()));
        unsafe impl<'a> Transient for B<'a> {
            type Static = B<'static>;
            type Transience = Contra<'a>;
        }
        assert_can_transend_to::<(A<'a>, B<'a>), Inv<'a>>();
    }
}
