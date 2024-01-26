/*!
This crate provides a mechanism for safely casting non-static data to/from `dyn Any`.


## Introduction

The standard library's `Any` trait is used to emulate dynamic typing within Rust,
and is extremely useful in cases where implementing a statically typed solution
would be inconvenient, if not impossible. Examples include storing heterogeneous
values in a `Vec`, or eliminating generic parameters from a type so that it can be
used in object-safe trait methods.

However, a significant limitation of the `Any` trait is its `'static` lifetime
bound, which prevents it from being used for types containing any non-`'static`
references. This bound eliminates many potential use-cases, and can force users
to sacrifice performance by cloning data that could be borrowed in others.

This crate aims to circumvent this limitation through careful use of unsafe
code hidden behind a safe abstraction, so that type-erasure may be applied
to transient (i.e. non-`'static`) data.


## Approach

The approach can be summarized as follows:

1. The `MakeStatic<'src>` trait is implemented/derived for a type, which is a simple
but `unsafe` trait that allows a transient type (or a reference to such) with minimum
lifetime bound `'src` to be transmuted into a `'static` version of the same type.
On its own, this operation would be extremely `unsafe`, but the following steps
will make use of the trait's `'src` lifetime parameter to build a safe abstraction.

2. The `'static`-ified type is then "erased" by casting to `dyn Any` (behind a box
or reference), which is now possible thanks to the falsely-`'static` lifetime.
However, using this object directly is still `unsafe`, as there is no lifetime
bounding access to any borrowed data it contains.

3. The erased value (or shared/mutable reference) is then wrapped in an `Erased`
(or `ErasedRef`/`ErasedMut`) struct, which binds the value to the true lifetime
`'src` using `PhantomData` to ensure that the borrowed data remains valid for
the lifetime of the wrapper. The API of this wrapper struct is designed such
that the wrapped value is not exposed in any safe public methods, and cannot be
extracted or referenced directly.

4. Finally, the `restore` method can be called to extract the value (or reference)
in its original form. This method attempts to downcast the erased value as the
given type, and then restores the original lifetime `'src` by calling another
method on the `MakeStatic` trait.


## Usage

After deriving or implementing the `MakeStatic` trait for a type, the primary
entry point for utilizing the functionality in this crate is provided by the
`TransientAny` trait, which has a blanket `impl` for all `T: MakeStatic<'src>`.
This trait exposes safe methods for erasing the type of an owned value, shared
reference, or mutable reference, each of which performs the necessary steps to
extend the value's lifetime, erase its type, and then place it in a wrapper to
keep it safe.

## Example

```
use transient_any::TransientAny;

#[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
struct S<'a> {
    value: &'a String,
}
# fn main() {
let string = "qwer".to_string();

// create a "transient" struct that borrows data
let original = S{value: &string};

// extend lifetime, erase type, and wrap with `Erased`
let erased = original.clone().into_erased();

// store in a vec with erased values of other types, etc.
/* ... */

// restore the static type and true lifetime
let restored = erased.restore::<S>().unwrap();
# }
```
*/

#[cfg(test)]
pub mod tests;

mod erased;
pub use erased::{Erased, ErasedRef, ErasedMut};

#[cfg(feature = "derive")]
pub use transient_any_derive::TransientAny;


/// Trait providing safe operations on `MakeStatic`  types.
///
/// This trait has a blanket impl for all `T: MakeStatic`, and cannot be
/// implemented directly.
pub trait TransientAny<'src>: MakeStatic<'src> {
    /// Erase the value's type and return a wrapper for safely restoring it.
    fn into_erased(self) -> Erased<'src> {
        Erased::new(self)
    }
    /// Erase the pointed-to value's type and return a wrapper for safely restoring it.
    fn as_erased<'borrow>(&'borrow self) -> ErasedRef<'borrow, 'src>
    where 'src: 'borrow {
        ErasedRef::new(self)
    }
    /// Erase the pointed-to value's type and return a wrapper for safely restoring it.
    fn as_erased_mut<'borrow>(&'borrow mut self) -> ErasedMut<'borrow, 'src>
    where 'src: 'borrow {
        ErasedMut::new(self)
    }
    fn type_id() -> std::any::TypeId {
        Self::static_type_id()
    }
}
impl<'src, T: MakeStatic<'src>> TransientAny<'src> for T {}


/// An unsafe trait for converting the lifetime parameters of a type to
/// (and from) `'static` so that it can be cast to `dyn Any`.
///
/// Only the associated type `Static` must be implemented, which must
/// be the same as `Self` but with all lifetime parameters replaced by
/// 'static. For example, a struct `S<'a, 'b, T>` should define the type
/// as `S<'static, 'static, T>`. For structs with zero or one lifetime
/// parameters, the included derive macro may be used to correctly
/// implement this trait.
///
/// SAFETY: `Self::Static` must be a static version of `Self` with the
/// same layout, and `'src` must match the shortest lifetime parameter
/// of the implementing struct.
pub unsafe trait MakeStatic<'src>: Sized + 'src {
    type Static: Sized + 'static;

    fn static_type_id() -> std::any::TypeId {
        std::any::TypeId::of::<Self::Static>()
    }

    /// SAFETY: the returned value must not be used after `'src`; in
    /// particular, it must not be cloned to produce an owned value
    /// borrowing data with a falsely 'static lifetime.
    unsafe fn make_static_owned(self: Box<Self>) -> Box<Self::Static> {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for `'src`
    unsafe fn from_static_owned(value: Box<Self::Static>) -> Box<Self> {
        std::mem::transmute(value)
    }

    /// SAFETY: the returned value must not be used after `'src`; in
    /// particular, it must not be cloned to produce an owned value
    /// borrowing data with a falsely 'static lifetime.
    unsafe fn make_static_ref(&self) -> &Self::Static {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for `'src`
    unsafe fn from_static_ref<'b>(value: &'b Self::Static) -> &'b Self {
        std::mem::transmute(value)
    }

    /// SAFETY: the returned value must not be used after `'src`; in
    /// particular, it must not be cloned to produce an owned value
    /// borrowing data with a falsely 'static lifetime.
    unsafe fn make_static_mut(&mut self) -> &mut Self::Static {
        std::mem::transmute(self)
    }

    /// SAFETY: the given value must be valid for `'src`
    unsafe fn from_static_mut<'b>(value: &'b mut Self::Static) -> &'b mut Self {
        std::mem::transmute(value)
    }
}
