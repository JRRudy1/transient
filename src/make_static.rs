use std::any::{Any, TypeId};

/// An unsafe trait for converting the lifetime parameters of a type to
/// (and from) 'static so that it can be cast to `dyn Any`.
///
/// Only the associated type `Static` must be implemented, which must
/// be the same as `Self` but with all lifetime parameters replaced by
/// 'static. For example, a struct `S<'a, 'b, T>` should define `Static`
/// as `S<'static, 'static, T>`.
///
/// SAFETY: `Self::Static` must be a static version of `Self` with the
/// same layout.
pub unsafe trait MakeStatic<'real>: Sized + 'real {
    type Static: Any + Sized + 'static;

    fn static_type_id() -> TypeId {
        TypeId::of::<Self::Static>()
    }

    /// SAFETY: the returned value must not be used after 'a
    unsafe fn make_static_owned(self: Box<Self>) -> Box<Self::Static> {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for 'a
    unsafe fn from_static_owned(value: Box<Self::Static>) -> Box<Self> {
        std::mem::transmute(value)
    }

    /// SAFETY: the returned value must not be used after 'a; in
    /// particular, it must not be cloned to produce an owned value
    /// borrowing data with a falsely 'static lifetime.
    unsafe fn make_static(&self) -> &Self::Static {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for 'a
    unsafe fn from_static(value: &Self::Static) -> &'real Self {
        std::mem::transmute(value)
    }

    /// SAFETY: the returned value must not be used after 'a; in
    /// particular, it must not be cloned to produce an owned value
    /// borrowing data with a falsely 'static lifetime.
    unsafe fn make_static_mut(&mut self) -> &mut Self::Static {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for 'a
    unsafe fn from_static_mut<'borrow>(value: &'borrow mut Self::Static) -> &'borrow mut Self
    where 'real: 'borrow
    {
        std::mem::transmute(value)
    }
}

/// Implements the `MakeStatic` trait for a a basic struct with
/// a single lifetime parameter and no type parameters.
#[allow(non_snake_case)]
#[macro_export]
macro_rules! derive_MakeStatic {
    ($name:ident<$lt:lifetime>) => {
        unsafe impl<$lt> MakeStatic<$lt> for $name<$lt> {
            type Static = $name<'static>;
        }
    };
}
// pub use derive_MakeStatic;
