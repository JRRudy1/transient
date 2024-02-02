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
(i.e. non-`'static`) data. This is achieved using the [`TransientAny`] trait, the
[`Erased`], [`ErasedRef`], and [`ErasedMut`] wrapper structs, and the
[`TransientAny` derive macro] that helps make the functionality
in this crate painless to utilize.

# Approach

The following steps are meant to illustrate what the crate does behind-the-scenes
to safely implement its functionality; skip to [the next section](#usage) if you
don't care and just want to learn about using it.

1. The [`TransientAny<'src>`] trait is implemented/derived for a type, which is a
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
First, the [`TransientAny`] trait is implemented for a type which may not have
generic lifetime or type parameters. This trait is already implemented for many
common foreign types, and can be easily implemented for custom types either by
hand or using the [`TransientAny` derive macro].

The trait's methods can then be called to safely erase a type by value ([`erase`]),
shared reference ([`erase_ref`]), or mutable reference ([`erase_mut`]). These
methods return the erased type wrapped in an [`Erased`], [`ErasedRef`], or
[`ErasedMut`] struct that maintains safety by binding to the original lifetime
and restricting access to the unbounded inner value it wraps.

The type-erased wrapper can then be used in dynamically typed patterns like
those enabled by the [`Any`] trait; but where `Box<dyn Any>` would be used
to erase the type of an owned value with a necessarily `'static` lifetime,
for example, you would instead use [`Erased<'a>`][`Erased`] and gain the
freedom to erase transient types containing non-`'static` references.

When dynamic typing is no longer needed, the wrapper's `restore` method
([`Erased::restore`], [`ErasedRef::restore`], or [`ErasedMut::restore`]) can
be called to move out of the wrapper and return the inner value/reference with
its static type and lifetime bounds restored.

# Examples

The following code block provides a basic example of using this crate to
utilize `Any`-like dynamic typing for a non-`'static` struct. Explicit type
annotations will be included for clarity, wherein the anonymous lifetime
`'_` will be used to represent the `'src` lifetime.

```rust
use transient_any::{TransientAny, Erased, Covariant, Invariant};

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
let erased: Erased<'_, Invariant<'_>> = original.erase();

let x = 4.erase();

// We can now do dynamically typed things with it, such as storing it in a
// `Vec` with other erased types:
let _ = vec![erased, 4.v_erase().into(), Box::new(2.0).v_erase().into()];
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

```rust
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
// borrow checker will continue to protect us by rejecting the next line:
drop(string);
# let _ = restored.clone();  // needed to prevent early drop
```

```rust
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

# Variance

Covariant:
```
use transient_any::*;
# fn main() {
fn compare<'a, V: Variance>(e1: &Erased<'a, V>, e2: &'a str) -> &'a str {
    e2
}
let static_str: &'static str = "static";
{
let short_string: String = "short".to_string();
let long = Erased::covariant(static_str);
 // 'long must shorten to 'short
// let x: &'_ str = compare(long, &short_string);
// assert_eq!(x, &short_string);
compare(&long, &short_string); // 'long must shorten to 'short (co)
let _ = format!("{:?}", long);
}
# }
```
Invariant:
```
use transient_any::*;
fn compare<'a, V: Variance>(e1: &Erased<'a, V>, e2: &Erased<'a, V>) -> &'a str {
    "qwer"
}
# fn main() {
let static_str: &'static str = "static";
let long: Erased<'_> = Erased::invariant(static_str);
{
let short_string: String = "short".to_string();
let short: Erased<'_, _> = Erased::invariant(&short_string);
let x: &'_ str = compare(&long, &short); // 'long must shorten to 'short (co)
let _ = format!("{:?}", short);
}
// let _ = format!("{:?}", long);
# }
```

Covariant2:
```
use transient_any::*;
fn shrink<'short, 'long: 'short>(long: ErasedCo<'long>, s: &'short String) -> ErasedCo<'short> {
    long
}
```
Invariant2:
```compile_fail
use transient_any::*;
fn shrink<'short, 'long: 'short>(long: ErasedInv<'long>, s: &'short String) -> ErasedInv<'short> {
    long
}
```


[`PhantomData`]: std::marker::PhantomData
[`Any`]: std::any::Any
[`dyn Any`]: https://doc.rust-lang.org/std/any/index.html#any-and-typeid
[`TransientAny`]: ../transient_any/trait.TransientAny.html
[`TransientAny<'src>`]: ../transient_any/trait.TransientAny.html
[`TransientAny` derive macro]: transient_any_derive::TransientAny
[`TransientAny` trait]: TransientAny
[`erase`]: TransientAny::erase
[`erase_ref`]: TransientAny::erase_ref
[`erase_mut`]: TransientAny::erase_mut
*/
// #![warn(missing_docs)]

#[cfg(test)]
pub mod tests;

pub mod erased;
pub mod variance;

pub mod any;
pub mod iter;


#[doc(inline)]
pub use crate::any::TransientAny;

#[doc(inline)]
pub use erased::{
    Erased, ErasedRef, ErasedMut,
    ErasedCo, ErasedInv,
};

pub use variance::{
    Variance, Invariant, Covariant, Static, VarianceTag
};

pub use iter::{
    IterErase
};

/// Re-exports symbols for covariant types
pub mod covariant {
    pub use crate::variance::{Variance, Covariant};
    pub use crate::erased::{ErasedCo as Erased};
}
/// Re-exports symbols for invariant structs
pub mod invariant {
    pub use crate::variance::{Variance, Invariant};
    pub use crate::erased::{ErasedInv as Erased};
}


#[cfg(feature = "derive")]
pub use transient_any_derive::TransientAny;



macro_rules! impl_primatives {
    ( $($ty:ty),* $(,)? ) => {
        $(
        unsafe impl<'src> TransientAny<'src> for $ty {
            type Static = $ty;
            type Variance = variance::Static;
        }
        )*
    }
}
macro_rules! impl_primative_refs {
    ( $($ty:ty),* $(,)? ) => {
        $(
        unsafe impl<'a> TransientAny<'a> for &'a $ty {
            type Static = &'static $ty;
            type Variance = variance::Covariant<'a>;
        }
        )*
    }
}
macro_rules! impl_primative_muts {
    ( $($ty:ty),* $(,)? ) => {
        $(
        unsafe impl<'a> TransientAny<'a> for &'a mut $ty {
            type Static = &'static mut $ty;
            type Variance = variance::Invariant<'a>;
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

unsafe impl<T: 'static> TransientAny<'static> for Vec<T> {
    type Static = Vec<T>;
    type Variance = Static;
}
unsafe impl<T: 'static> TransientAny<'static> for Box<T> {
    type Static = Box<T>;
    type Variance = Static;
}
unsafe impl<T: 'static> TransientAny<'static> for Box<[T]> {
    type Static = Box<[T]>;
    type Variance = Static;
}
unsafe impl<T: 'static> TransientAny<'static> for Option<T> {
    type Static = Option<T>;
    type Variance = Static;
}
unsafe impl<T: 'static, E: 'static> TransientAny<'static> for Result<T, E> {
    type Static = Result<T, E>;
    type Variance = Static;
}

unsafe impl TransientAny<'static> for Box<dyn std::any::Any> {
    type Static = Box<dyn std::any::Any>;
    type Variance = Static;
}
unsafe impl<'a> TransientAny<'a> for &'a dyn std::any::Any {
    type Static = &'static dyn std::any::Any;
    type Variance = Covariant<'a>;
}
unsafe impl<'a> TransientAny<'a> for &'a mut dyn std::any::Any {
    type Static = &'static mut dyn std::any::Any;
    type Variance = Invariant<'a>;
}
