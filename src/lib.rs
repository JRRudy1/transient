//! This library provides a mechanism for safely casting non-static data to/from `dyn Any`.

// TODO: reason about advanced cases (contravariance, multithreading, etc.)


#[cfg(test)]
pub mod tests;

mod erased;

pub use erased::{Erased, ErasedRef, ErasedMut};

#[cfg(feature = "derive")]
pub use transient_any_derive::MakeStatic;


/// Trait providing safe operations on `MakeStatic`  types.
pub trait Erase<'src>: MakeStatic<'src> {
    /// Erase the value's type and return an interface for safely restoring it.
    fn into_erased(self) -> Erased<'src> {
        Erased::new(self)
    }
    /// Erase the pointed-to value's type and return an interface for safely restoring it.
    fn as_erased<'borrow>(&'borrow self) -> ErasedRef<'borrow, 'src>
    where 'src: 'borrow {
        ErasedRef::new(self)
    }
    /// Erase the pointed-to value's type and return an interface for safely restoring it.
    fn as_erased_mut<'borrow>(&'borrow mut self) -> ErasedMut<'borrow, 'src>
    where 'src: 'borrow {
        ErasedMut::new(self)
    }
    fn type_id() -> std::any::TypeId {
        std::any::TypeId::of::<Self::Static>()
    }
}
impl<'src, T: MakeStatic<'src>> Erase<'src> for T {}


/// An unsafe trait for converting the lifetime parameters of a type to
/// (and from) 'static so that it can be cast to `dyn Any`.
///
/// Only the associated type `Static` must be implemented, which must
/// be the same as `Self` but with all lifetime parameters replaced by
/// 'static. For example, a struct `S<'a, 'b, T>` should define `Static`
/// as `S<'static, 'static, T>`.
///
/// SAFETY: `Self::Static` must be a static version of `Self` with the
/// same layout, and `'src` must match the shortest lifetime parameter
/// of the implementing struct.
pub unsafe trait MakeStatic<'src>: Sized + 'src {
    type Static: Sized + 'static;

    fn static_type_id() -> std::any::TypeId {
        std::any::TypeId::of::<Self::Static>()
    }

    /// SAFETY: the returned value must not be used after 'src
    unsafe fn make_static_owned(self: Box<Self>) -> Box<Self::Static> {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for 'src
    unsafe fn from_static_owned(value: Box<Self::Static>) -> Box<Self> {
        std::mem::transmute(value)
    }

    /// SAFETY: the returned value must not be used after 'src; in
    /// particular, it must not be cloned to produce an owned value
    /// borrowing data with a falsely 'static lifetime.
    unsafe fn make_static_ref(&self) -> &Self::Static {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for 'src
    unsafe fn from_static_ref<'b>(value: &'b Self::Static) -> &'b Self {
        std::mem::transmute(value)
    }

    /// SAFETY: the returned value must not be used after 'src; in
    /// particular, it must not be cloned to produce an owned value
    /// borrowing data with a falsely 'static lifetime.
    unsafe fn make_static_mut(&mut self) -> &mut Self::Static {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for 'src
    // unsafe fn from_static_mut<'borrow>(value: &'borrow mut Self::Static) -> &'borrow mut Self
    unsafe fn from_static_mut<'b>(value: &'b mut Self::Static) -> &'b mut Self
    {
        std::mem::transmute(value)
    }
}
