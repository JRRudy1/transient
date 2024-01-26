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

use std::{
    any::{Any, TypeId}, 
    marker::PhantomData
};
use super::MakeStatic;


/// Safely wraps a potentially non-static value that has been transmuted
/// to `'static` and cast to `dyn Any`.
///
/// This struct provides a safe interface for erasing/restoring such a type by
/// restricting access to the falsely-`'static` object and ensuring that it cannot
/// be used after its *true* lifetime. To enforce this condition, the safe public
/// API does not expose the wrapped value directly, which in principle could be
/// downcast and cloned to obtain a transient value with an unbounded lifetime.
#[derive(Debug)]
pub struct Erased<'src>(
    Box<dyn Any>,  // DO NOT EXPOSE!
    PhantomData<&'src ()>
);

impl<'src> Erased<'src> {

    pub fn new<T: MakeStatic<'src>>(value: T) -> Self {
        Self::from_boxed(Box::new(value))
    }

    pub fn from_boxed<T: MakeStatic<'src>>(boxed: Box<T>) -> Self {
        let extended: Box<T::Static> = unsafe {boxed.make_static_owned()};
        Self(extended, PhantomData)
    }

    /// Safely restore the original type and lifetime of the wrapped value.
    /// 
    /// If the conversion fails, the `Erased` object is rebuilt and returned 
    /// in the `Err` variant so that the caller may regain ownership.
    pub fn restore<T: MakeStatic<'src>>(self) -> Result<T, Self> {
        let restored = self.0.downcast::<T::Static>()
            .map_err(|inner| Erased(inner, PhantomData))?;
        // the lifetime of the pointed-to value must have been 'src
        // for `self` to be created from safe code
        let shortened: Box<T> = unsafe {T::from_static_owned(restored)};
        Ok(*shortened)
    }

    pub fn type_id(&self) -> TypeId {
        (&*self.0).type_id()
    }

    pub fn is<T: MakeStatic<'src>>(&self) -> bool {
        (&*self.0).is::<T::Static>()
    }
}

/// If the wrapped object is `'static` we can safely expose it.
impl Erased<'static> {
    pub fn inner(&self) -> &dyn Any {
        &*self.0
    }
    pub fn inner_mut(&mut self) -> &mut dyn Any {
        &mut *self.0
    }
    pub fn into_inner(self) -> Box<dyn Any> {
        self.0
    }
}


/// Safely wraps a shared reference to a potentially non-static value
/// that has been transmuted to `'static` and cast to `dyn Any`.
///
/// This struct provides a safe interface for erasing/restoring such a type by
/// restricting access to the falsely-`'static` object and ensuring that it cannot
/// be used after its *true* lifetime. To enforce this condition, the safe public
/// API does not expose the wrapped value directly, which in principle could be
/// downcast and cloned to obtain a transient value with an unbounded lifetime.
#[derive(Debug, Clone, Copy)]
pub struct ErasedRef<'borrow, 'src: 'borrow>(
    &'borrow dyn Any, // DO NOT EXPOSE!
    PhantomData<&'src ()>,
);

impl<'borrow, 'src: 'borrow> ErasedRef<'borrow, 'src> {

    pub fn new<T: MakeStatic<'src>>(value: &'borrow T) -> Self {
        Self(unsafe {value.make_static_ref()}, PhantomData)
    }

    /// Safely restore the original type and lifetime of the wrapped value.
    pub fn restore<T: MakeStatic<'src>>(self) -> Result<&'borrow T, String> {
        let restored = self.0.downcast_ref::<T::Static>()
            .ok_or("invalid type".to_string())?;
        // the true lifetime must have been 'src for `self` to be created from safe code
        let shortened = unsafe {T::from_static_ref(restored)};
        Ok(shortened)
    }

    pub fn type_id(&self) -> TypeId {
        self.0.type_id()
    }

    pub fn is<T: MakeStatic<'src>>(&self) -> bool {
        self.0.is::<T::Static>()
    }
}

/// If the wrapped object is `'static` we can safely expose it.
impl<'borrow> ErasedRef<'borrow, 'static> {
    pub fn inner(&self) -> &dyn Any {
        &*self.0
    }
}


/// Safely wraps a mutable reference to a potentially non-static value
/// that has been transmuted to `'static` and cast to `dyn Any`.
///
/// This struct provides a safe interface for erasing/restoring such a type by
/// restricting access to the falsely-`'static` object and ensuring that it cannot
/// be used after its *true* lifetime. To enforce this condition, the safe public
/// API does not expose the wrapped value directly, which in principle could be
/// downcast and cloned to obtain a transient value with an unbounded lifetime.
#[derive(Debug)]
pub struct ErasedMut<'borrow, 'src: 'borrow>(
    &'borrow mut dyn Any, // DO NOT EXPOSE!
    PhantomData<&'src ()>,
);

impl<'borrow, 'src: 'borrow> ErasedMut<'borrow, 'src> {

    pub fn new<T: MakeStatic<'src>>(value: &'borrow mut T) -> Self {
        Self(unsafe {value.make_static_mut()}, PhantomData)
    }

    /// Safely restore the original type and lifetime of the wrapped value.
    pub fn restore<T: MakeStatic<'src>>(self) -> Result<&'borrow mut T, String> {
        let restored = self.0.downcast_mut::<T::Static>()
            .ok_or("invalid type".to_string())?;
        // the true lifetime must have been 'src for `self` to be created from safe code
        let shortened = unsafe {T::from_static_mut(restored)};
        Ok(shortened)
    }

    pub fn type_id(&self) -> TypeId {
        (&*self.0).type_id()
    }

    pub fn is<T: MakeStatic<'src>>(&self) -> bool {
        (&*self.0).is::<T::Static>()
    }
}

/// If the wrapped object is `'static` we can safely expose it.
impl<'borrow> ErasedMut<'borrow, 'static> {
    pub fn inner(&self) -> &dyn Any {
        &*self.0
    }
    pub fn inner_mut(&mut self) -> &mut dyn Any {
        &mut *self.0
    }
}
