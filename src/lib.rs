/*!
This crate extends the dynamic typing mechanism provided by the [`std::any::Any`]
trait to add support for types with non-`'static` lifetimes.

# Introduction

The standard library's [`Any`] trait is used to emulate dynamic typing within
Rust, and is extremely useful in cases where implementing a statically typed
solution would be inconvenient, if not impossible. Examples include storing
heterogeneous values in a `Vec`, or eliminating generic parameters from a
type so that it can be used in object-safe trait methods.

However, a significant limitation of the `Any` trait is its `'static` lifetime
bound, which prevents it from being used for types containing any non-`'static`
references. This restriction eliminates many potential use-cases, and in others
it can force users to sacrifice performance by cloning data that could otherwise
be borrowed.

This crate aims to circumvent this limitation through careful use of `unsafe` code
hidden behind a safe abstraction, so that type-erasure may be applied to transient
(i.e. non-`'static`) data. This is achieved using the [`MakeStatic`] and
[`TransientAny`] traits, the [`Erased`], [`ErasedRef`], and [`ErasedMut`] wrapper
structs, and the [`TransientAny` derive macro] that helps make the functionality
in this crate painless to utilize.

# Approach

The following steps are meant to illustrate what the crate does behind-the-scenes
to safely implement its functionality; skip to [the next section](#usage) if you
don't care and just want to learn about using it.

1. The [`MakeStatic<'src>`] trait is implemented/derived for a type, which is a
simple but `unsafe` trait that allows a transient type (or a reference to such)
with minimum lifetime bound `'src` to be transmuted into a `'static` version of
the same type. On its own, this operation would be extremely `unsafe`, but the
following steps will make use of the trait's `'src` lifetime parameter to build
a safe abstraction.

2. The `'static`-ified type is then *erased* by casting to [`dyn Any`] (behind
a box or reference), which is now possible thanks to the falsely-`'static`
lifetime. However, using this object directly is still dangerous as there is
no lifetime bounding access to the borrowed data it contains.

3. The erased value (or shared/mutable reference) is then wrapped in an [`Erased`]
(or [`ErasedRef`]/[`ErasedMut`]) struct, which uses [`PhantomData`] to bind the
value to its true lifetime `'src` and ensure that the borrowed data remains valid
for the lifetime of the wrapper. Furthermore, the API of this wrapper struct is
designed such that the wrapped value is *not* exposed in any safe public methods,
and cannot be extracted or referenced directly.

4. Finally, each wrapper provides a `restore<T>` method that can be called to
extract the value (or reference) in its original form. This method attempts to
downcast the erased value as the given type `T`, and then restores the original
lifetime `'src` before returning it to the caller.

# Usage
After implementing the [`MakeStatic`] trait for a type (either manually or
using the [`TransientAny` derive macro]), the primary entry point for utilizing
the functionality in this crate is provided by the [`TransientAny` trait].
This trait is implemented automatically by a blanket `impl` for all
`T: MakeStatic<'src>`, and exposes safe methods for erasing the type of an owned
value ([`erase`]), shared reference ([`erase_ref`]), or mutable reference
([`erase_mut`]). Each of these methods performs the necessary steps to extend
the value's lifetime, erase its type, and place it in a wrapper that maintains
safety by binding to the original lifetime and restricting access to the
unbounded inner value.

When dynamic typing is no longer needed, the wrapper's `restore` method
([`Erased::restore`], [`ErasedRef::restore`], or [`ErasedMut::restore`])
can be called to move out of the wrapper and return the inner value/reference
with its static type and lifetime bounds restored.

# Examples

The following code block provides a basic example of using this crate to
utilize `Any`-like dynamic typing for a non-`'static` struct. Explicit type
annotations will be included for clarity, wherein the anonymous lifetime
`'_` will be used to represent the `'src` lifetime.
```
use transient_any::{TransientAny, Erased};

#[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
struct S<'a> {
    value: &'a String,
}

// Create a `String` that the `S` struct will borrow. The lifetime of this
// string ultimately defines the `'src` lifetime, and all references to it
// must be dropped before it goes out of scope.
let string = "qwer".to_string();

// Create a "transient" struct that borrows the string:
let original: S<'_> = S{value: &string};

// Extend lifetime, erase type, and wrap with an `Erased` struct to preserve
// and enforce its lifetime bound:
let erased: Erased<'_> = original.erase();

// We can now do dynamically typed things with it, such as storing it in a
// `Vec` with other erased types:
let _ = vec![erased, 4.erase(), Box::new(2.0).erase()];
```

```compile_fail
# use transient_any::TransientAny;
# #[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
# struct S<'a> {value: &'a String} let string = "qwer".to_string();
# let original = S{value: &string}; let erased = original.erase();
// Even though the `S` struct's lifetime has been transmuted to `'static`,
// the wrapper keeps it bound to the true lifetime and the borrow checker
// would reject the next line to protect us from use-after-free:
drop(string);
# let _ = erased.clone();  // needed to prevent early drop
```

```
# use transient_any::{TransientAny, Erased};
# #[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
# struct S<'a> {value: &'a String} let string = "qwer".to_string();
# let original = S{value: &string}; let erased = original.clone().erase();
// Restore the static type and lifetime of the transient struct:
let restored: S<'_> = erased.restore().unwrap();
assert_eq!(&restored, &original);
```

```compile_fail
# use transient_any::TransientAny;
# #[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
# struct S<'a> {value: &'a String} let string = "qwer".to_string();
# let original = S{value: &string}; let erased = original.erase();
# let restored: S = erased.restore().unwrap();
// Now that the true lifetime of the inner value has been restored, the
// borrow checker will continue to protect us and reject the next line:
drop(string);
# let _ = restored.clone();  // needed to prevent early drop
```

```
# use transient_any::{TransientAny, Erased};
# #[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
# struct S<'a> {value: &'a String} let string = "qwer".to_string();
# let original = S{value: &string}; let erased = original.erase();
# let restored: S = erased.restore().unwrap();
// However, once `restored` gets dropped the borrow is released and we
// are free to drop or move the string again!
drop(restored);
drop(string);
```

[`PhantomData`]: std::marker::PhantomData
[`Any`]: std::any::Any
[`dyn Any`]: https://doc.rust-lang.org/std/any/index.html#any-and-typeid
[`MakeStatic`]: ../transient_any/trait.MakeStatic.html
[`MakeStatic<'src>`]: ../transient_any/trait.MakeStatic.html
[`TransientAny` derive macro]: transient_any_derive::TransientAny
[`TransientAny` trait]: TransientAny
[`erase`]: TransientAny::erase
[`erase_ref`]: TransientAny::erase_ref
[`erase_mut`]: TransientAny::erase_mut
*/

#[cfg(test)]
pub mod tests;

pub mod erased;
#[doc(inline)]
pub use erased::{Erased, ErasedRef, ErasedMut};

#[cfg(feature = "derive")]
pub use transient_any_derive::TransientAny;


/// Trait providing safe operations on [`MakeStatic`] types, including
/// [`erase`], [`erase_ref`], and [`erase_mut`].
///
/// This trait has a blanket `impl` for all `T: MakeStatic`, and cannot be
/// implemented directly. Instead, implement the `MakeStatic` trait (e.g.
/// using the [`TransientAny` derive macro]).
///
/// [`erase`]: TransientAny::erase
/// [`erase_ref`]: TransientAny::erase_ref
/// [`erase_mut`]: TransientAny::erase_mut
/// [`TransientAny` derive macro]: transient_any_derive::TransientAny
pub trait TransientAny<'src>: MakeStatic<'src> {

    /// Erase the value's type and return a wrapper for safely restoring it.
    fn erase(self) -> Erased<'src> {
        Erased::new(self)
    }
    /// Erase the pointed-to value's type and return a wrapper for safely restoring it.
    fn erase_ref<'borrow>(&'borrow self) -> ErasedRef<'borrow, 'src>
    where 'src: 'borrow {
        ErasedRef::new(self)
    }
    /// Erase the pointed-to value's type and return a wrapper for safely restoring it.
    fn erase_mut<'borrow>(&'borrow mut self) -> ErasedMut<'borrow, 'src>
    where 'src: 'borrow {
        ErasedMut::new(self)
    }
    /// Get the [TypeId][`std::any::TypeId`] of the `'static`-ified type.
    fn static_type_id() -> std::any::TypeId {
        std::any::TypeId::of::<Self::Static>()
    }
}
impl<'src, T: MakeStatic<'src>> TransientAny<'src> for T {}


/// Unsafe trait for converting the lifetime parameters of a type to (and from)
/// `'static` so that it can be cast to `dyn Any`. This trait can be derived
/// using the [`TransientAny` derive macro][transient_any_derive::TransientAny].
///
/// Only the associated type `Static` must be implemented, which must be the
/// same as `Self` but with all lifetime parameters replaced by `'static`.
/// For example, a struct `S<'a, 'b, T>` should define the type as
/// `S<'static, 'static, T>`.
///
/// For structs with zero or one lifetime parameters, the included derive
/// macro [`TransientAny`][transient_any_derive::TransientAny] can be used to
/// automatically implement this trait. For structs with multiple lifetime
/// parameters the trait can easily implemented by hand, but care must be
/// taken to ensure that the struct's shortest lifetime is used as the
/// lifetime parameter for the trait.
///
/// # SAFETY
/// - The [`Static`][Self::Static] associated type should be the same as the
/// `Self` type but with all lifetime parameters replaced by `'static`.
/// - The trait's lifetime parameter `'src` must match the shortest lifetime
/// parameter declared for the `impl` block.
///
/// # Examples
/// For the simple case of a struct with a single lifetime parameter:
/// ```
/// use transient_any::MakeStatic;
/// struct S<'a> {
///     value: &'a str,
/// }
/// unsafe impl<'a> MakeStatic<'a> for S<'a> {
///     type Static = S<'static>;
/// }
/// ```
///
/// Now consider a struct that borrows 2 string slices with independent
/// lifetime parameters:
/// ```
/// struct TwoRefs<'a, 'b> {
///     a: &'a str,
///     b: &'b str,
/// }
/// ```
/// For the `MakeStatic` implementation to be sound, a sufficient relationship
/// between the lifetime parameters must be declared on the `impl` block so that
/// a *minimum lifetime* can be unambiguously chosen to parameterize the trait.
///
/// There are three acceptable choices for the lifetime relationships in the
/// `TwoRefs` struct declared above:
/// ```
/// # use transient_any::MakeStatic;
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// // 'b outlives 'a -> choose 'a for the trait
/// unsafe impl<'a, 'b: 'a> MakeStatic<'a> for TwoRefs<'a, 'b> {
///     type Static = TwoRefs<'static, 'static>;
/// }
/// ```
/// ```
/// # use transient_any::MakeStatic;
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// // 'a outlives 'b -> choose `b` for the trait
/// unsafe impl<'b, 'a: 'b> MakeStatic<'b> for TwoRefs<'a, 'b> {
///     type Static = TwoRefs<'static, 'static>;
/// }
/// ```
/// ```
/// # use transient_any::MakeStatic;
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// // 'a and 'b are equal -> no need to choose
/// unsafe impl<'a> MakeStatic<'a> for TwoRefs<'a, 'a> {
///     type Static = TwoRefs<'static, 'static>;
/// }
/// ```
/// However, choosing either `'a` **or** `'b` for the trait without declaring
/// bounds to justify it is *unsound* and may lead to undefined behaviour.
///
pub unsafe trait MakeStatic<'src>: Sized + 'src {
    type Static: Sized + 'static;

    /// # SAFETY
    /// The returned value must not be used after `'src`; in particular it must
    /// not be cloned to produce an owned value borrowing data with an unbounded
    /// lifetime.
    unsafe fn make_static_owned(self: Box<Self>) -> Box<Self::Static> {
        std::mem::transmute(self)
    }

    /// # SAFETY
    /// The given value must be valid for `'src`.
    unsafe fn from_static_owned(value: Box<Self::Static>) -> Box<Self> {
        std::mem::transmute(value)
    }

    /// # SAFETY
    /// The pointee of the returned reference must not be accessed after
    /// `'src`; in particular, the reference must not be cloned to produce
    /// an owned value borrowing data with an unbounded lifetime.
    unsafe fn make_static_ref(&self) -> &Self::Static {
        std::mem::transmute(self)
    }

    /// # SAFETY
    /// The pointee of the given reference must be valid for `'src`.
    unsafe fn from_static_ref<'b>(value: &'b Self::Static) -> &'b Self {
        std::mem::transmute(value)
    }

    /// # SAFETY
    /// The pointee of the returned reference must not be accessed after
    /// `'src`; in particular, the reference must not be cloned to produce
    /// an owned value borrowing data with an unbounded lifetime.
    unsafe fn make_static_mut(&mut self) -> &mut Self::Static {
        std::mem::transmute(self)
    }

    /// # SAFETY
    /// The pointee of the given reference must be valid for `'src`.
    unsafe fn from_static_mut<'b>(value: &'b mut Self::Static) -> &'b mut Self {
        std::mem::transmute(value)
    }
}


macro_rules! impl_primatives {
    ( $($ty:ty),* $(,)? ) => {
        $(
        unsafe impl MakeStatic<'static> for $ty {
            type Static = $ty;
        }
        )*
    }
}
macro_rules! impl_primative_refs {
    ( $($ty:ty),* $(,)? ) => {
        $(
        unsafe impl<'a> MakeStatic<'a> for &'a $ty {
            type Static = &'static $ty;
        }
        )*
    }
}
macro_rules! impl_primative_muts {
    ( $($ty:ty),* $(,)? ) => {
        $(
        unsafe impl<'a> MakeStatic<'a> for &'a mut $ty {
            type Static = &'static mut $ty;
        }
        )*
    }
}


impl_primatives!{
    isize, i8, i16, i32, i64, usize, u8, u16, u32, u64, f32, f64,
    String, Box<str>, &'static str,
}
impl_primative_refs!{
    isize, i8, i16, i32, i64, usize, u8, u16, u32, u64, f32, f64,
    String, Box<str>, &'static str,
}
impl_primative_muts!{
    isize, i8, i16, i32, i64, usize, u8, u16, u32, u64, f32, f64,
    String, Box<str>, &'static str,
}

unsafe impl<T: 'static> MakeStatic<'static> for Vec<T> {
    type Static = Vec<T>;
}
unsafe impl<T: 'static> MakeStatic<'static> for Box<T> {
    type Static = Box<T>;
}
unsafe impl<T: 'static> MakeStatic<'static> for Box<[T]> {
    type Static = Box<[T]>;
}
unsafe impl<T: 'static> MakeStatic<'static> for Option<T> {
    type Static = Option<T>;
}
unsafe impl<T: 'static, E: 'static> MakeStatic<'static> for Result<T, E> {
    type Static = Result<T, E>;
}

unsafe impl MakeStatic<'static> for Box<dyn std::any::Any> {
    type Static = Box<dyn std::any::Any>;
}

unsafe impl<'a> MakeStatic<'a> for &'a dyn std::any::Any {
    type Static = &'static dyn std::any::Any;
}

unsafe impl<'a> MakeStatic<'a> for &'a mut dyn std::any::Any {
    type Static = &'static mut dyn std::any::Any;
}
