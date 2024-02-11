/*!
Defines safe wrappers for erasing and restoring transient types.

This module defines the `Erased`, `ErasedRef`, and `ErasedMut` structs for
wrapping owned values, shared references, and mutable references, respectively,
that have been transmuted to `'static` and cast to `dyn Any`. While artificially
extending lifetimes is typically very unsafe, each wrapper struct provides a
safe interface to the falsely-`'static` value it wraps by restricting safe
access to it until the *true* lifetime has been restored.

In order to enforce this restriction, the safe public API does not expose the
wrapped value directly, which in principle could be downcast and cloned to
obtain a transient value with an unbounded lifetime. However, this restriction
is lifted when the *true* lifetime is static, since use-after-free is no longer
a concern.
*/
#![allow(missing_docs)]

use std::{
    any::{Any, TypeId},
    marker::PhantomData,
    mem,
};
use crate::{Co, Inv, Contra};
use super::{
    transient::{Transient, Static as _},
    storage::{Storage, Owned, Ref, Mut, BoxVec},
    transience::{
        Transience, IntoTransience, Static,
        Invariant, Covariant, Contravariant, Variance,
    }
};


/// Safely wraps a potentially non-`'static` value that has been transmuted
/// to `'static` and cast to `Box<dyn Any>`.
///
/// This struct provides a safe interface for erasing/restoring such a type by
/// restricting access to the falsely-`'static` object and ensuring that it cannot
/// be used after its *true* lifetime `'src`. To enforce this condition, the safe
/// public API does not expose the wrapped value directly, which in principle could
/// be downcast and cloned to obtain a transient value with an unbounded lifetime.
///
///# Lifetimes
/// - `'src`: Lifetime for which the source type is valid.
/// - `'b`: Lifetime for which the type is borrowed, which must be shorter
/// than or equal to `'src`. For owned values, this should be equal to `'src`.
///
/// The lifetimes need to be independent so that `T: 'long` can be borrowed
/// for `'short` but still restored to the original `'long`.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct ErasedBase<Storage: ?Sized, Transience: ?Sized> (
    PhantomData<Transience>, // lifetime bindings
    Storage, // `Storage` implementation holding the `dyn Any`
);


/*
#[allow(dead_code)]
mod split {
    use super::*;
    use crate::cell::{TransientCell, construct_cell};
    use crate::storage::{box_static_from};

    #[repr(transparent)]
    pub struct OwnedStatic<T: Transient, R: Transience> (
        TransientCell<Box<T::Static>, R>,
    );
    impl<T: Transient, R: Transience> OwnedStatic<T, R> {
        pub fn erase(self) -> ErasedCell<R> {
            unsafe { ErasedCell(self.0.map(|v| v as Box<dyn Any>)) }
        }
        pub fn as_erased(&self) -> ErasedRef<'_, R> {
            unsafe { ErasedRef::construct(self.0.inner()) }
        }
        pub fn as_erased_mut(&mut self) -> ErasedMut<'_, R> {
            unsafe { ErasedMut::construct(self.0.inner_mut()) }
        }
    }
    impl<T: Transient> OwnedStatic<T, T::Transience> {
        pub fn new(value: T) -> Self {
            let boxed = Box::new(value);
            unsafe {
                Self(construct_cell(box_static_from(boxed)))
            }
        }
    }

    #[repr(transparent)]
    pub struct ErasedCell<R: Transience> (
        TransientCell<Box<dyn Any>, R>
    );
    impl<R: Transience> ErasedCell<R> {

        pub fn restore<T: Transient<Transience=R>>(self) -> Result<T, Self> {
            unsafe {
                let restored: Box<T> = self.0
                    .into_inner()
                    .downcast::<T::Static>()
                    .map_err(|inner| Self(construct_cell(inner)))?
                    .restore_box::<T>()
                ;
                Ok(*restored)
            }
        }

    }


}
*/ // todo!
pub type Erased<Transience> = ErasedBase<Owned, Transience>;
pub type ErasedRef<'b, Transience> = ErasedBase<Ref<'b>, Transience>;
pub type ErasedMut<'b, Transience> = ErasedBase<Mut<'b>, Transience>;

pub type ErasedVec<Transience> = ErasedBase<BoxVec, Transience>;

pub type ErasedStatic = Erased<Static>;
pub type ErasedCo<'src> = Erased<Covariant<'src>>;
pub type ErasedContra<'src> = Erased<Contravariant<'src>>;
pub type ErasedInv<'src> = Erased<Invariant<'src>>;


/// variance methods
impl<'b, S: Storage<'b>, V> ErasedBase<S, V> {

    pub fn inew<'a, T>(value: S::Input<T>) -> ErasedBase<S, Invariant<'a>>
    where
        T: Transient<Transience=V>,
        V: Variance<'a>,
    {
        unsafe { ErasedBase::construct(S::erase(value)) }
    }
    pub fn into_inv<'a, T>(self) -> ErasedBase<S, Invariant<'a>>
    where
        T: Transient<Transience=V>,
        V: Variance<'a>,
    {
        self.into_transience()
    }
    pub fn inv<'a, T>(&self) -> &ErasedBase<S, Invariant<'a>>
    where
        T: Transient<Transience=V>,
        V: Variance<'a>,
    {
        self.as_transience::<Invariant<'a>>()
    }
    pub fn inv_mut<'a, T>(&mut self) -> &mut ErasedBase<S, Invariant<'a>>
    where
        T: Transient<Transience=V>,
        V: Variance<'a>,
    {
        self.as_transience_mut()
    }
}

/// generic methods
impl<'b, S, R> ErasedBase<S, R>
{
    /// # Safety
    /// The data must be compatible with the requested transience.
    #[inline(always)]
    pub(crate) const unsafe fn construct(storage: S) -> Self {
        ErasedBase(PhantomData, storage)
    }
}

/// generic methods
impl<'b, S: Storage<'b>, R: Transience> ErasedBase<S, R>
{

    /// Erase and wrap a `Transient` value with a transience compatible with `R`.
    ///
    /// This is equivalent to calling the `Transient::erase` trait method.
    /// To explicitly request an *invariant* wrapper, call `Erased::invariant`
    /// instead.
    pub fn new<T>(value: S::Input<T>) -> Self
    where
        T: Transient,
        T::Transience: IntoTransience<R>,
    {
        unsafe { Self::construct(S::erase(value)) }
    }
    pub fn invariant<T: Transient<Transience=R>>(value: S::Input<T>) -> ErasedBase<S, R::Frozen> {
        unsafe { ErasedBase::construct(S::erase(value)) }
    }

    /// Safely restore the type and lifetime of the wrapped value.
    ///
    /// If the conversion fails, `self` is returned in the `Err` variant so
    /// the caller can regain ownership.
    pub fn restore<T: Transient<Transience=R>>(self) -> Result<S::Input<T>, Self> {
        match unsafe { self.1.restore::<T>() } {
            Ok(restored) => Ok(restored),
            Err(storage) => Err( unsafe{ Self::construct(storage) })
        }
    }

    /// Get the `TypeId` of the wrapped value (see [`Any::type_id`]).
    pub fn type_id(&self) -> TypeId {
        unsafe { self.1.as_dyn().type_id() }
    }

    /// Check whether the wrapped value has the given type (see
    /// [`<dyn Any>::is`](https://doc.rust-lang.org/std/any/trait.Any.html#method.is)).
    ///
    /// Note that this comparison does not take the lifetime of `T` into account; an
    /// erased value with original type `&'src str` will return `true` when compared to
    /// any `&'_ str` including `&'static str`, even though they would typically be
    /// classified as distinct types.
    pub fn is<T: Transient<Transience=R>>(&self) -> bool {
        unsafe { self.1.as_dyn().is::<T::Static>() }
    }

    /// todo
    pub fn into_invariant(self) -> ErasedBase<S, R::Frozen> {
        self.into_transience::<R::Frozen>()
    }

    pub fn into_transience<R2>(self) -> ErasedBase<S, R2>
    where
        R2: Transience,
        R: IntoTransience<R2>,
    {
        // the trait bound guarantees compatability
        unsafe{ ErasedBase::construct(self.1) }
    }

    /// Get a view of this wrapper with the parameterizing variance.
    ///
    /// # SAFETY
    /// The returned reference must not be used in any variance shenanigans that
    /// are not appropriate for the true variance of the erased type.
    pub fn as_transience<R2>(&self) -> &ErasedBase<S, R2>
    where
        R2: Transience,
        R: IntoTransience<R2>
    {
        // The variance is a ZST so they have the same layout
        unsafe { mem::transmute(self) }
    }

    /// Get a view of this wrapper with the parameterizing variance.
    ///
    /// # SAFETY
    /// The returned reference must not be used in any variance shenanigans that
    /// are not appropriate for the true variance of the erased type.
    pub fn as_transience_mut<R2>(&mut self) -> &mut ErasedBase<S, R2>
    where
        R2: Transience,
        R: IntoTransience<R2>
    {
        // The variance is a ZST so they have the same layout
        unsafe { mem::transmute(self) }
    }

    /// # SAFETY
    /// The returned wrapper must not be used in any variance shenanigans that
    /// are not appropriate for the true variance of `T`.
    pub unsafe fn into_variance_forced<R2>(self) -> ErasedBase<S, R2>
    where
        R2: Transience,
    {
        ErasedBase::construct(self.1)
    }

    /// Get a view of this wrapper with the parameterizing variance.
    ///
    /// # SAFETY
    /// The returned reference must not be used in any variance shenanigans that
    /// are not appropriate for the true variance of the erased type.
    pub unsafe fn as_variance_forced<R2: Transience>(&self) -> &ErasedBase<S, R2> {
        // The variance is a ZST so they have the same layout
        mem::transmute(self)
    }
}



/// owned methods
impl<V: Transience> ErasedBase<Owned, V>
{
    pub fn as_ref(&self) -> ErasedBase<Ref, V> {
        unsafe{ ErasedBase::construct(&*self.1) }
    }
    pub fn as_mut(&mut self) -> ErasedBase<Mut, V> {
        unsafe{ ErasedBase::construct(&mut *self.1) }
    }
}
/// borrowed methods
impl<'b, V> ErasedRef<'b, V> {

    pub fn as_owned(&self) -> &Erased<V> {
        let refref: &'_ &'b dyn Any = &self.1;
        // The following sequence of congruences justifies the transmutation:
        // `&&dyn Any` => `&Box<dyn Any>` => `&Owned` => `&Erased<_>`. The
        // other concern when transmuting to a `Box` involves it trying to
        // deallocate its contents, but we are keeping it behind a reference.
        unsafe { mem::transmute(refref) }
    }

    pub fn as_ref(&self) -> Self {
        unsafe { ErasedRef::construct(self.1) }
    }
}
/// mutably borrowed methods
impl<'b, V: Transience> ErasedBase<Mut<'b>, V>
{
    pub fn as_ref(&self) -> ErasedBase<Ref, V> {
        unsafe{ ErasedBase::construct(&*self.1) }
    }
    pub fn as_owned(&mut self) -> &mut ErasedBase<Owned, V> {
        let refref: &'_ mut &'b mut dyn Any = &mut self.1;
        unsafe { mem::transmute(refref) }
    }
}


// === METHODS FOR ACCESSING THE WRAPPED VALUE WHEN `'src: 'static` === //

/// These methods are only implemented when `'src: 'static`, since access to
/// a transient value with an artificially extended lifetime would be unsafe.
impl<S> ErasedBase<S, Static> {
    /// Get a shared reference to the wrapped `dyn Any`; only implemented
    /// when the original value was `'static`.
    pub fn inner(&self) -> &S {
        &self.1
    }
    /// Get a mutable reference to the wrapped `dyn Any`; only implemented
    /// when the original value was `'static`.
    pub fn inner_mut(&mut self) -> &mut S {
        &mut self.1
    }
    /// Move out of the wrapper and return the wrapped `Box<dyn Any>`; only
    /// implemented when the original value was `'static`.
    pub fn into_inner(self) -> S {
        self.1
    }
}


// === CONVERSION METHODS === //
/*
/// Static => Invariant<'any> (always valid)
impl<'src> From<Erased<Static>> for Erased<Invariant<'src>> {
    fn from(value: Erased<Static>) -> Self {
        value.into_transience()
        // // make covariant
        // let covariant: Erased<Covariant<'static>> = value.into_covariant();
        // // covariance lets the lifetime shorten
        // let shortened: Erased<Covariant<'src>> = covariant;
        // // anything can become invariant
        // shortened.into_invariant()
    }
}

/// Static => Covariant<'any> (always valid)
impl<'src> From<Erased<Static>> for Erased<Covariant<'src>> {
    fn from(value: Erased<Static>) -> Self {
        value.into_transience()
        // // statics are always valid so variance doesn't matter
        // unsafe { value.into_variance()}
    }
}

/// Covariant<'src> => Invariant<'src>
impl<'src> From<Erased<Covariant<'src>>> for Erased<Invariant<'src>> {
    fn from(value: Erased<Covariant<'src>>) -> Self {
        value.into_invariant()
    }
}
*/

// === Deref Impls === //
/*
impl Deref for Erased<'static, Static> {
    type Target = Erased<'static, Invariant<'static>>;
    fn deref(&self) -> &Self::Target {
        self.as_invariant()
    }
}
impl<'src> Deref for Erased<'src, Covariant<'src>> {
    type Target = Erased<'src, Invariant<'src>>;
    fn deref(&self) -> &Self::Target {
        self.as_invariant()
    }
}*/

// === CONVERSIONS TO/FROM `dyn Any` === //
/*

/

/// Since a `dyn Any` created from safe code has an implicit `'static` bound,
/// we can place it directly into the wrapper with `'src: 'static`. However,
/// note that the `Erased::restore` method will restore the original value
/// that was cast to `dyn Any`, not the `Box<dyn Any>` itself.
impl From<Box<dyn Any>> for Erased<'static, Static> {
    fn from(value: Box<dyn Any>) -> Self {
        Erased(value, PhantomData)
    }
}

/// Since a `dyn Any` created from safe code has an implicit `'static` bound,
/// we can place it directly into the wrapper with `'src: 'static`. However,
/// note that the `ErasedRef::restore` method will restore the original value
/// that was cast to `dyn Any`, not the `&dyn Any` itself.
impl<'borrow> From<&'borrow dyn Any> for ErasedRef<'borrow, 'static> {
    fn from(value: &'borrow dyn Any) -> Self {
        ErasedRef(value, PhantomData)
    }
}

/// Since a `dyn Any` created from safe code has an implicit `'static` bound,
/// we can place it directly into the wrapper with `'src: 'static`. However,
/// note that the `ErasedRef::restore` method will restore the original value
/// that was cast to `dyn Any`, not the erased `dyn Any` itself.
impl<'borrow> From<&'borrow mut dyn Any> for ErasedRef<'borrow, 'static> {
    fn from(value: &'borrow mut dyn Any) -> Self {
        ErasedRef(&*value, PhantomData)
    }
}

/// Since a `dyn Any` created from safe code has an implicit `'static` bound,
/// we can place it directly into the wrapper with `'src: 'static`. However,
/// note that the `ErasedMut::restore` method will restore the original value
/// that was cast to `dyn Any`, not the erased `dyn Any` itself.
impl<'borrow> From<&'borrow mut dyn Any> for ErasedMut<'borrow, 'static> {
    fn from(value: &'borrow mut dyn Any) -> Self {
        ErasedMut(value, PhantomData)
    }
}
*/
