//! This library provides a mechanism for safely casting non-static data to/from `dyn Any`.
#![allow(unused_macros, dead_code)]

#[macro_use]
mod make_static;
mod erased;

#[cfg(test)]
pub mod tests;


pub use make_static::{MakeStatic};
pub use erased::{Erased, ErasedRef, ErasedMut};

#[cfg(feature = "derive")]
pub use transient_any_derive::MakeStatic;


/// Trait providing safe operations on `MakeStatic`  types.
pub trait Erase<'real>: MakeStatic<'real> {
    /// Erase the value's type and return an interface for safely restoring it.
    fn into_erased(self) -> Erased<'real> {
        Erased::new(self)
    }
    /// Erase the pointed-to value's type and return an interface for safely restoring it.
    fn as_erased(&'real self) -> ErasedRef<'real> {
        ErasedRef::new(self)
    }
    /// Erase the pointed-to value's type and return an interface for safely restoring it.
    fn as_erased_mut<'b>(&'b mut self) -> ErasedMut<'b, 'real> where 'real: 'b {
        ErasedMut::new(self)
    }
}
impl<'a, T: MakeStatic<'a>> Erase<'a> for T {}
