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
    // ops::Deref,
    // fmt::Debug
};
// use std::mem::transmute;
use super::{TransientAny, Variance, Invariant, Covariant, Static};


/// Trait
///
/// # Lifetimes
/// - `'src`: Lifetime for which the source type is valid.
/// - `'b`: Lifetime for which the type is borrowed, which must be shorter
/// than or equal to `'src`. For owned values, this should be equal to `'src`.
///
/// # Safety
/// `Storage` implementations are not intended to provide a safe API, and
/// must be placed in a wrapper that protects it from misuse. As such, they
/// should never be released to safe code.
pub trait Storage<'b>: Sized {
    type Input<'src: 'b, T: TransientAny<'src>>: Sized;
    unsafe fn erase<'src: 'b, T: TransientAny<'src>>(value: Self::Input<'src, T>) -> Self;
    unsafe fn restore<'src: 'b, T: TransientAny<'src>>(self) -> Result<Self::Input<'src, T>, Self>;
}
pub type Owned = Box<dyn Any>;
pub type Borrowed<'b> = &'b dyn Any;

impl<'b> Storage<'b> for Owned {
    type Input<'src: 'b, T: TransientAny<'src>> = T;
    /// # Safety
    /// The returned storage is *unprotected* and must be used carefully
    /// so that it is not released to safe code. In particular, it must not
    /// be used after `'src`, which the borrow checker will not enforce due
    /// to the falsely `'static` lifetime.
    unsafe fn erase<'src: 'b, T: TransientAny<'src>>(value: T) -> Self {
        let boxed = Box::new(value);
        let extended: Box<T::Static> = unsafe {mem::transmute(boxed)};
        extended as Box<dyn Any>
    }
    // todo
    unsafe fn restore<'src: 'b, T: TransientAny<'src>>(self) -> Result<T, Self> {
        let restored: Box<T::Static> = self.downcast()?;
        let shortened: Box<T> = unsafe {mem::transmute(restored)};
        Ok(*shortened)
    }
}
impl<'b> Storage<'b> for Borrowed<'b> {
    type Input<'src: 'b, T: TransientAny<'src>> = &'b T;
    // todo
    unsafe fn erase<'src: 'b, T: TransientAny<'src>>(value: &'b T) -> Self {
        let extended: &'b T::Static = unsafe {mem::transmute(value)};
        extended as &'b dyn Any
    }
    // todo
    unsafe fn restore<'src: 'b, T: TransientAny<'src>>(self) -> Result<&'b T, Self> {
        match self.downcast_ref::<T::Static>() {
            Some(restored) => {
                let shortened: &T = unsafe {mem::transmute(restored)};
                Ok(shortened)
            },
            None => Err(self)
        }
    }
}

///# Lifetimes
/// - `'src`: Lifetime for which the source type is valid.
/// - `'b`: Lifetime for which the type is borrowed, which must be shorter
/// than or equal to `'src`. For owned values, this should be equal to `'src`.
///
/// The lifetimes need to be independent so that `T: 'long` can be borrowed
/// for `'short` but still restored to the original `'long`.
#[derive(Debug)]
#[repr(transparent)]
pub struct Erased2<'src, V, S> (
    S,  // `Storage` implementation holding the `dyn Any`
    PhantomData<&'src V>,
);
/// generic methods
impl<'b, 'src: 'b, V, S: Storage<'b>> Erased2<'src, V, S>
{
    pub fn new<T: TransientAny<'src>>(value: S::Input<'src, T>) -> Self {
        let erased = unsafe { S::erase(value) };
        Erased2(erased, PhantomData)
    }
    pub fn restore<T: TransientAny<'src>>(self) -> Result<S::Input<'src, T>, Self> {
        match unsafe { self.0.restore::<T>() } {
            Ok(restored) => Ok(restored),
            Err(storage) => Err(Self(storage, PhantomData))
        }
    }
}

/// owned methods
impl<'src, V> Erased2<'src, V, Owned>
{
    pub fn as_ref<'b>(&'b self) -> Erased2<'src, V, Borrowed<'b>>
        where 'src: 'b
    {
        Erased2(&*self.0, PhantomData)
    }
}
/// borrowed methods
impl<'b, 'src: 'b, V> Erased2<'src, V, Borrowed<'b>>
{
    pub fn as_ref(&self) -> &Erased2<'src, V, Owned> {
        let refref: &'_ &'b dyn Any = &self.0;
        // `&&dyn Any` has the same layout as `&Box<dyn Any>`, which is
        // synonymous with `&Owned`; since `Erased` is `repr(transparent)`,
        // `&Erased<_, Owned>` will have the same layout as well. We can
        // thus safely transmute between them. The other consideration is
        // the fake `Box` trying to deallocate its contents, but it is kept
        // behind a reference so this won't happen.
        unsafe { mem::transmute(refref) }
    }
}




/// Safely wraps a potentially non-`'static` value that has been transmuted
/// to `'static` and cast to `Box<dyn Any>`.
///
/// This struct provides a safe interface for erasing/restoring such a type by
/// restricting access to the falsely-`'static` object and ensuring that it cannot
/// be used after its *true* lifetime `'src`. To enforce this condition, the safe 
/// public API does not expose the wrapped value directly, which in principle could 
/// be downcast and cloned to obtain a transient value with an unbounded lifetime.
#[derive(Debug)]
#[repr(transparent)]
pub struct Erased<'src, V: Variance + 'src = Invariant<'src>> (
    Box<dyn Any>, // DO NOT EXPOSE!
    PhantomData<&'src V>,
);

pub type ErasedCo<'a> = Erased<'a, Covariant<'a>>;
pub type ErasedInv<'a> = Erased<'a, Invariant<'a>>;


// todo: remove junk
impl<'src, V: Variance> Erased<'src, V> {
    /// Requires adding a level of indirection and leaking a fat pointer :(
    #[allow(unused, dead_code)]
    fn from_ref<'b, T: TransientAny<'src>>(value: &'b T) -> &'b Self {
        // `TransientAny` promises `T` and `T::Static` have the same layout,
        // and lengthening the lifetime is ok b/c we'll protect it
        let extended: &'b T::Static = unsafe { mem::transmute(value) };
        // Erase the type by casting to `dyn Any`
        let erased: &'b dyn Any = extended;
        // We need one more level of indirection to match that of `&Self`. We
        // need to use `Box` for this second level so that the inner pointer
        // gets moved to the heap where it can live long enough for us to keep
        // a reference to it.
        let boxed: Box<&'b dyn Any> = Box::new(erased);
        // Now we leak the box to convert the outer pointer from a `Box` to a
        // regular reference. Note that the allocation pointed to by the `Box`
        // no longer has an owner to deallocate it, and might be leaked.
        let leaked: & &'b dyn Any = &*Box::leak(boxed);
        // `Box<T, Allocator=Global>` is guaranteed to have the same layout
        // as `&T` so transmuting is fine. The box will stay behind a reference
        // so we don't have to worry about it trying to deallocate its contents.
        let inverted: &Box<dyn Any> = unsafe { mem::transmute(leaked) };
        // The `repr(transparent)` attribute on `Erased` guarantees that it
        // has the same layout as `Box<dyn Any`.
        let c: &Self = unsafe { mem::transmute(inverted) };
        c
    }


}


impl<'src> Erased<'src, Invariant<'src>> {
    /// Similar to [`Erased::new`], but always returns an *invariant* wrapper.
    ///
    /// This is equivalent to calling the `TransientAny::ierase` trait method.
    pub fn invariant<T: TransientAny<'src>>(value: T) -> Erased<'src, Invariant<'src>> {
        Erased::new(value).into_invariant()
    }
}

pub unsafe trait SupportsCo<'src>: Variance {}
unsafe impl<'src> SupportsCo<'src> for Covariant<'src> {}
unsafe impl<'any> SupportsCo<'any> for Static {}

impl<'src> Erased<'src, Covariant<'src>> {
    /// Similar to [`Erased::new`], but always returns an *invariant* wrapper.  // todo
    ///
    /// This is equivalent to calling the `TransientAny::ierase` trait method.
    pub fn covariant<T: TransientAny<'src>>(value: T) -> Erased<'src, Covariant<'src>>
    where
        T::Variance: SupportsCo<'src>
    {
        // `SupportsCo` trait bound guarantees covariant
        unsafe { Erased::new(value).into_variance() }
    }
}

impl<'src, V: Variance + SupportsCo<'src>> Erased<'src, V> {

    /// todo
    pub fn into_covariant(self) -> Erased<'src, Covariant<'src>> {
        // `SupportsCo` impl bound guarantees covariant
        unsafe { self.into_variance() }
    }
}

impl<'any> Erased<'any, Static> {

    /// todo
    pub fn into_other<'other, V: Variance>(self) -> Erased<'other, V> {
        // `Static` can covert to any variance or lifetime
        unsafe { mem::transmute(self) }
    }
}



impl<'src, V: Variance> Erased<'src, V> {

    /// Erase and wrap a transient value with lifetime `'src` and variance
    /// `T::Variance`.
    ///
    /// This is equivalent to calling the `TransientAny::erase` trait method.
    /// To explicitly request an *invariant* wrapper, call `Erased::invariant`
    /// instead.
    pub fn new<T>(value: T) -> Erased<'src, V>
    where
        T: TransientAny<'src, Variance = V>
    {
        let boxed = Box::new(value);
        // transmuting is safe because `TransientAny` promises the types are
        // compatible; lengthening the lifetime is safe because this wrapper
        // forbids (safe) access to the unbounded value
        let extended: Box<T::Static> = unsafe {mem::transmute(boxed)};
        Erased(extended, PhantomData)
    }

    /// Erase and wrap a transient value with lifetime `'src` and variance `V`.
    ///
    /// This constructor *does not* respect the variance defined by `T::Variance`,
    /// and instead matches the variance `V` for the impl (typically inferred at
    /// the call site). This makes the method `unsafe`, as choosing the wrong
    /// variance can lead to UB in some cases.
    ///
    /// To get a wrapper with variance determined by `T::Variance` instead, call
    /// the `Erased::new` method. To explicitly request an *invariant* wrapper,
    /// call the `Erased::invariant` method.
    ///
    /// # SAFETY
    /// The returned wrapper must not be used in any variance shenanigans that are
    /// not appropriate for the true variance of `T`.
    pub unsafe fn with_variance<T>(value: T) -> Erased<'src, V>
    where
        T: TransientAny<'src, Variance=V> // variance not specified
    {
        let boxed = Box::new(value);
        // transmuting is safe because `TransientAny` promises the types are
        // compatible; lengthening the lifetime is safe because this wrapper
        // forbids (safe) access to the unbounded value
        let extended: Box<T::Static> = unsafe {mem::transmute(boxed)};
        Erased(extended, PhantomData)
    }

    /// Safely restore the type and lifetime of the wrapped value.
    ///
    /// If the conversion fails, `self` is returned in the `Err` variant so 
    /// the caller can regain ownership. To return the value without unboxing, 
    /// call [`restore_box`][Erased::restore_box] instead.
    pub fn restore<T: TransientAny<'src>>(self) -> Result<T, Self> {
        Ok(*self.restore_box::<T>()?)
    }

    /// Safely restore the type and lifetime of the wrapped value and return 
    /// it in a `Box`.
    ///
    /// If the conversion fails, `self` is returned in the `Err` variant so 
    /// the caller can regain ownership. To return the value unboxed, call 
    /// [`restore`][Erased::restore] instead.
    pub fn restore_box<T: TransientAny<'src>>(self) -> Result<Box<T>, Self> {
        let restored: Box<T::Static> = self.0.downcast()
            .map_err(|inner| Erased(inner, PhantomData))?;
        // safe because `TransientAny<'src>` promises that the types are
        // compatible and the true lifetime is `'src`
        let shortened: Box<T> = unsafe {mem::transmute(restored)};
        Ok(shortened)
    }

    /// Get the `TypeId` of the wrapped value (see [`Any::type_id`]).
    pub fn type_id(&self) -> TypeId {
        (&*self.0).type_id()
    }

    /// Check whether the wrapped value has the given type (see
    /// [`<dyn Any>::is`](https://doc.rust-lang.org/std/any/trait.Any.html#method.is)).
    ///
    /// Note that this comparison does not take the lifetime of `T` into account; an
    /// erased value with original type `&'src str` will return `true` when compared to
    /// any `&'_ str` including `&'static str`, even though they would typically be
    /// classified as distinct types.
    pub fn is<T: TransientAny<'src>>(&self) -> bool {
        (&*self.0).is::<T::Static>()
    }

    /// Get a safely wrapped shared reference to the erased type.
    pub fn as_ref(&self) -> ErasedRef<'_, 'src> {
        // we know that `ErasedRef` will uphold the same invariants
        ErasedRef(&*self.0, PhantomData)
    }

    /// Get a safely wrapped mutable reference to the erased type.
    pub fn as_mut(&mut self) -> ErasedMut<'_, 'src> {
        // we know that `ErasedMut` will uphold the same invariants
        ErasedMut(&mut *self.0, PhantomData)
    }

    /// Convert `self` into an *invariant* version.
    pub fn into_invariant(self) -> Erased<'src, Invariant<'src>> {
        // invariance is safe to use for all types.
        unsafe { self.into_variance::<Invariant>() }
    }

    /// Get an *invariant* view of this wrapper.
    pub fn as_invariant(&self) -> &Erased<'src, Invariant<'src>> {
        // invariance is safe to use for all types.
        unsafe { self.as_variance::<Invariant>() }
    }

    /// Get a view of this wrapper with the parameterizing variance.
    ///
    /// # SAFETY
    /// The returned reference must not be used in any variance shenanigans that
    /// are not appropriate for the true variance of the erased type.
    pub unsafe fn as_variance<V2: Variance>(&self) -> &Erased<'src, V2> {
        // The variance is a ZST so they have the same layout
        unsafe {mem::transmute(self)}
    }

    /// Replace the wrapper's variance with the parameterizing type.
    ///
    /// # SAFETY
    /// The returned wrapper must not be used in any variance shenanigans that
    /// are not appropriate for the true variance of the erased type.
    pub unsafe fn into_variance<V2: Variance>(self) -> Erased<'src, V2> {
        // The variance is a ZST so they have the same layout
        unsafe {mem::transmute(self)}
    }
}


/// Safely wraps a shared reference to a potentially non-`'static` value that
/// has been transmuted to `'static` and cast to `&dyn Any`.
///
/// This struct provides a safe interface for erasing/restoring such a type by
/// restricting access to the falsely-`'static` object and ensuring that it cannot
/// be used after its *true* lifetime `'src`. To enforce this condition, the safe
/// public API does not expose the wrapped value directly, which in principle
/// could be downcast and cloned to obtain a transient value with an unbounded
/// lifetime.
#[derive(Debug, Clone, Copy)]
pub struct ErasedRef<'borrow, 'src: 'borrow>(
    &'borrow dyn Any, // DO NOT EXPOSE!
    PhantomData<fn(&'src ()) -> &'src ()>,
    // PhantomData<&'src ()>,
);

impl<'borrow, 'src: 'borrow> ErasedRef<'borrow, 'src> {

    /// Erase and wrap a shared reference to a value with lifetime `'src` that
    /// has been borrowed with lifetime `'borrow`. This is equivalent to calling
    /// the `TransientAny::erase_ref` trait method.
    pub fn new<T: TransientAny<'src>>(value: &'borrow T) -> Self {
        // transmuting is safe because `TransientAny` promises the types are
        // compatible; lengthening the lifetime is safe because this wrapper
        // forbids (safe) access to the unbounded value
        let extended: &T::Static = unsafe {mem::transmute(value)};
        Self(extended, PhantomData)
    }

    /// Safely restore the original type `T` and lifetime `'src` of the pointee.
    ///
    /// If the conversion fails, `self` is returned in the `Err`
    /// variant so that the caller can regain ownership.
    pub fn restore<T: TransientAny<'src>>(self) -> Result<&'borrow T, Self> {
        let restored: &T::Static = self.0.downcast_ref().ok_or(self)?;
        // safe because `TransientAny<'src>` promises that the types are
        // compatible and the true lifetime is `'src`
        let shortened: &T = unsafe {mem::transmute(restored)};
        Ok(shortened)
    }

    /// Get the `TypeId` of the wrapped value (see [`Any::type_id`]).
    pub fn type_id(&self) -> TypeId {
        self.0.type_id()
    }

    /// Check whether the pointee of the wrapped reference has the given type (see
    /// [`<dyn Any>::is`](https://doc.rust-lang.org/std/any/trait.Any.html#method.is)).
    ///
    /// Note that this comparison does not take the lifetime of `T` into account; an
    /// erased value with original type `&'src str` will return `true` when compared to
    /// any `&'_ str` including `&'static str`, even though they would typically be
    /// classified as distinct types.
    pub fn is<T: TransientAny<'src>>(&self) -> bool {
        self.0.is::<T::Static>()
    }
}

/// Safely wraps a mutable reference to a potentially non-`'static` value that
/// has been transmuted to `'static` and cast to `&mut dyn Any`.
///
/// This struct provides a safe interface for erasing/restoring such a type by
/// restricting access to the falsely-`'static` object and ensuring that it cannot
/// be used after its *true* lifetime `'src`. To enforce this condition, the safe public
/// API does not expose the wrapped value directly, which in principle could be
/// downcast and cloned to obtain a transient value with an unbounded lifetime.
#[derive(Debug)]
pub struct ErasedMut<'borrow, 'src: 'borrow>(
    &'borrow mut dyn Any, // DO NOT EXPOSE!
    PhantomData<&'src ()>,
);

impl<'borrow, 'src: 'borrow> ErasedMut<'borrow, 'src> {

    /// Erase and wrap a mutable reference to a value with lifetime `'src` that
    /// has been borrowed with lifetime `'borrow`. This is equivalent to calling
    /// the `TransientAny::erase_mut` trait method.
    pub fn new<T: TransientAny<'src>>(value: &'borrow mut T) -> Self {
        // transmuting is safe because `TransientAny` promises the types are
        // compatible; lengthening the lifetime is safe because this wrapper
        // forbids (safe) access to the unbounded value
        let extended: &mut T::Static = unsafe {mem::transmute(value)};
        Self(extended, PhantomData)        
        /*Self(unsafe {value.make_static_mut()}, PhantomData)*/
    }

    /// Safely restore the original type `T` and lifetime `'src` of the pointee.
    ///
    /// If the conversion fails, `self` is returned in the `Err` variant so
    /// the caller can regain ownership.
    pub fn restore<T: TransientAny<'src>>(self) -> Result<&'borrow mut T, Self> {
        if !self.is::<T>() {
            return Err(self)
        }
        let restored: &mut T::Static = self.0.downcast_mut().unwrap();
        // `safe because `TransientAny<'src>` promises that the types are
        // compatible and the true lifetime is `'src`
        let shortened: &mut T = unsafe {mem::transmute(restored)};
        Ok(shortened)
        /*// the true lifetime must have been 'src for `self` to be created from safe code
        Ok(unsafe {T::from_static_mut(restored)})*/
    }

    /// Get the `TypeId` of the wrapped value (see [`Any::type_id`])
    pub fn type_id(&self) -> TypeId {
        (&*self.0).type_id()
    }

    /// Check whether the pointee of the wrapped reference has the given type (see
    /// [`<dyn Any>::is`](https://doc.rust-lang.org/std/any/trait.Any.html#method.is)).
    ///
    /// Note that this comparison does not take the lifetime of `T` into account; an
    /// erased value with original type `&'src str` will return `true` when compared to
    /// any `&'_ str` including `&'static str`, even though they would typically be
    /// classified as distinct types.
    pub fn is<T: TransientAny<'src>>(&self) -> bool {
        (&*self.0).is::<T::Static>()
    }

    /// Reborrow the wrapped reference as a (safely wrapped) shared reference
    pub fn as_ref(&self) -> ErasedRef<'_, 'src> {
        // we know that `ErasedRef` will uphold the same invariants
        ErasedRef(&*self.0, PhantomData)
    }
}

// === METHODS FOR ACCESSING THE WRAPPED VALUE WHEN `'src: 'static` === //

/// These methods are only implemented when `'src: 'static`, since access to
/// a transient value with an artificially extended lifetime would be unsafe.
impl Erased<'static, Static> {
    /// Get a shared reference to the wrapped `dyn Any`; only implemented
    /// when the original value was `'static`.
    pub fn inner(&self) -> &dyn Any {
        &*self.0
    }
    /// Get a mutable reference to the wrapped `dyn Any`; only implemented
    /// when the original value was `'static`.
    pub fn inner_mut(&mut self) -> &mut dyn Any {
        &mut *self.0
    }
    /// Move out of the wrapper and return the wrapped `Box<dyn Any>`; only
    /// implemented when the original value was `'static`.
    pub fn into_inner(self) -> Box<dyn Any> {
        self.0
    }
}

/// These methods are only implemented when `'src: 'static`, since access to
/// a transient value with an artificially extended lifetime would be unsafe.
impl<'borrow> ErasedRef<'borrow, 'static> {
    /// Get a shared reference to the wrapped `dyn Any`; only implemented
    /// when the original value was `'static`.
    pub fn inner(&self) -> &dyn Any {
        &*self.0
    }
}

/// These methods are only implemented when `'src: 'static`, since access to
/// a transient value with an artificially extended lifetime would be unsafe.
impl<'borrow> ErasedMut<'borrow, 'static> {
    /// Get a shared reference to the wrapped `dyn Any`; only implemented
    /// when the original value was `'static`.
    pub fn inner(&self) -> &dyn Any {
        &*self.0
    }
    /// Get a mutable reference to the wrapped `dyn Any`; only implemented
    /// when the original value was `'static`.
    pub fn inner_mut(&mut self) -> &mut dyn Any {
        &mut *self.0
    }
}

// === CONVERSION METHODS === //

// Static => Invariant<'any> (always valid)
impl<'src, 'any> Into<Erased<'any, Invariant<'any>>> for Erased<'src, Static> {
    fn into(self) -> Erased<'any, Invariant<'any>> {
        self.into_other()
    }
}
// Static => Covariant<'any> (always valid)
impl<'src, 'any> Into<Erased<'any, Covariant<'any>>> for Erased<'src, Static> {
    fn into(self) -> Erased<'any, Covariant<'any>> {
        self.into_other()
    }
}

// Covariant<'src> => Invariant<'src>
impl<'src, 'any> Into<Erased<'src, Invariant<'src>>> for Erased<'src, Covariant<'src>> {
    fn into(self) -> Erased<'src, Invariant<'src>> {
        self.into_invariant()
    }
}



// Static
// impl<'src> From<Erased<'static, Static>> for Erased<'src, Invariant<'src>> {
//     fn from(value: Erased<'static, Static>) -> Self {
//         // make covariant
//         let covariant: Erased<'static, Covariant<'static>> = value.into_covariant();
//         // covariance lets the lifetime shorten
//         let shortened: Erased<'src, Covariant<'src>> = covariant;
//         // anything can become invariant
//         shortened.into_invariant()
//     }
// }
// impl<'src> From<Erased<'static, Static>> for Erased<'src, Covariant<'src>> {
//     fn from(value: Erased<'static, Static>) -> Self {
//         // statics are always valid so variance doesn't matter
//         unsafe { value.into_variance()}
//     }
// }
// impl<'src> From<Erased<'src, Covariant<'src>>> for Erased<'src, Invariant<'src>> {
//     fn from(value: Erased<'src, Covariant<'src>>) -> Self {
//         value.into_invariant()
//     }
// }



//
// impl Deref for Erased<'static, Static> {
//     type Target = Erased<'static, Invariant<'static>>;
//     fn deref(&self) -> &Self::Target {
//         self.as_invariant()
//     }
// }
// impl<'src> Deref for Erased<'src, Covariant<'src>> {
//     type Target = Erased<'src, Invariant<'src>>;
//     fn deref(&self) -> &Self::Target {
//         self.as_invariant()
//     }
// }


// === CONVERSIONS TO/FROM `dyn Any` === //

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
