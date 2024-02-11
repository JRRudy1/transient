transient
=====
This crate extends the dynamic typing mechanism provided by the [`std::any::Any`]
trait to add support for types with non-`'static` lifetimes.

# Introduction

The standard library's `Any` trait is used to emulate dynamic typing within
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
(i.e. non-`'static`) data. This is achieved using the `MakeStatic` and
`Transient` traits, the `Erased`, `ErasedRef`, and `ErasedMut` wrapper
structs, and the `Transient` derive macro that helps make the functionality
in this crate painless to utilize.

# Approach

The following steps are meant to illustrate what the crate does behind-the-scenes
to safely implement its functionality; skip to [the next section](#usage) if you
don't care and just want to learn about using it.

1. The `MakeStatic<'src>` trait is implemented/derived for a type, which is a
simple but `unsafe` trait that allows a transient type (or a reference to such)
with minimum lifetime bound `'src` to be transmuted into a `'static` version of
the same type. On its own, this operation would be extremely `unsafe`, but the
following steps will make use of the trait's `'src` lifetime parameter to build
a safe abstraction.

2. The `'static`-ified type is then *erased* by casting to `dyn Any` (behind
a box or reference), which is now possible thanks to the falsely-`'static`
lifetime. However, using this object directly is still dangerous as there is
no lifetime bounding access to the borrowed data it contains.

3. The erased value (or shared/mutable reference) is then wrapped in an `Erased`
(or `ErasedRef`/`ErasedMut`) struct, which uses `PhantomData` to bind the
value to its true lifetime `'src` and ensure that the borrowed data remains valid
for the lifetime of the wrapper. Furthermore, the API of this wrapper struct is
designed such that the wrapped value is *not* exposed in any safe public methods,
and cannot be extracted or referenced directly.

4. Finally, each wrapper provides a `restore<T>` method that can be called to
extract the value (or reference) in its original form. This method attempts to
downcast the erased value as the given type `T`, and then restores the original
lifetime `'src` before returning it to the caller.

# Usage
After implementing the `MakeStatic` trait for a type (either manually or
using the `Transient` derive macro]), the primary entry point for utilizing
the functionality in this crate is provided by the `Transient` trait].
This trait is implemented automatically by a blanket `impl` for all
`T: MakeStatic<'src>`, and exposes safe methods for erasing the type of an owned
value (`erase`), shared reference (`erase_ref`), or mutable reference
(`erase_mut`). Each of these methods performs the necessary steps to extend
the value's lifetime, erase its type, and place it in a wrapper that maintains
safety by binding to the original lifetime and restricting access to the
unbounded inner value.

When dynamic typing is no longer needed, the wrapper's `restore` method
(`Erased::restore`, `ErasedRef::restore`, or `ErasedMut::restore`)
can be called to move out of the wrapper and return the inner value/reference
with its static type and lifetime bounds restored.

# Examples

The following code block provides a basic example of using this crate to
utilize `Any`-like dynamic typing for a non-`'static` struct. Explicit type
annotations will be included for clarity, wherein the anonymous lifetime
`'_` will be used to represent the `'src` lifetime.

```rust
use transient::{Transient, Erased};

#[derive(Transient, Clone, Debug, PartialEq, Eq)]
struct S<'a> {
    value: &'a String,
}

fn main() {
    // Create a `String` that the `S` struct will borrow:
    let string = "qwer".to_string();
    
    // Create a transient struct that borrows the string:
    let original: S<'_> = S{value: &string};
    
    // Extend lifetime, erase type, and wrap with an `Erased` struct to preserve
    // and enforce its lifetime bound:
    let erased: Erased<'_> = original.erase();
    
    // We can now do dynamically typed things with it, such as storing it in a
    // `Vec` with other erased types:
    let _ = vec![erased, 4.erase(), Box::new(2.0).erase()];
    
    // Restore the static type and lifetime of the transient struct:
    let restored: S<'_> = erased.restore().unwrap();
    assert_eq!(&restored, &original);
}
```

[`PhantomData`]: https://doc.rust-lang.org/std/marker/struct.PhantomData.html
[`std::any::Any`]: https://doc.rust-lang.org/std/any/trait.Any.html