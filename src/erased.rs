//! Defines wrapper structs providing a safe interface for erased objects.
use std::{
    any::{Any, TypeId}, 
    marker::PhantomData
};
use super::MakeStatic;


/// Wraps a (potentially) non-static object that has been
/// transmuted to 'static and cast to `dyn Any`. This struct provides a
/// safe interface for erasing/restoring such a type by restricting
/// access to the falsely-static object and ensuring that it cannot be
/// used after its true lifetime. To enforce this condition, the safe
/// public API does not expose the wrapped reference directly, which
/// in principle could be downcast and then cloned to produce an owned
/// value capable of invoking UB from safe code (by using it after the
/// true lifetime).
#[derive(Debug)]
pub struct Erased<'src>(Box<dyn Any>, PhantomData<&'src ()>);

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

/// Wraps a reference to a (potentially) non-static object that has been
/// transmuted to 'static and cast to `dyn Any`. This struct provides a
/// safe interface for erasing/restoring such a type by restricting
/// access to the falsely-static object and ensuring that it cannot be
/// used after its true lifetime. To enforce this condition, the safe
/// public API does not expose the wrapped reference directly, which
/// in principle could be downcast and then cloned to produce an owned
/// value capable of invoking UB from safe code (by using it after the
/// true lifetime).
#[derive(Debug, Clone, Copy)]
pub struct ErasedRef<'borrow, 'src: 'borrow>(
    &'borrow dyn Any,
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

/// Wraps a reference to a (potentially) non-static object that has been
/// transmuted to 'static and cast to `dyn Any`. This struct provides a
/// safe interface for erasing/restoring such a type by restricting
/// access to the falsely-static object and ensuring that it cannot be
/// used after its true lifetime. To enforce this condition, the safe
/// public API does not expose the wrapped reference directly, which
/// in principle could be downcast and then cloned to produce an owned
/// value capable of invoking UB from safe code (by using it after the
/// true lifetime).
#[derive(Debug)]
pub struct ErasedMut<'borrow, 'src: 'borrow>(
    &'borrow mut dyn Any,
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