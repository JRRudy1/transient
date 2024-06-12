//! This crate re-implements the dynamic typing mechanism provided by the
//! [`std::any::Any`] trait to add support for types with non-`'static` lifetimes.
//!
//! # Introduction
//! The standard library's [`Any`] trait is used to emulate dynamic typing within
//! Rust, and is extremely useful in cases where implementing a statically typed
//! solution would be inconvenient, if not impossible. Examples include storing
//! heterogeneous values in a `Vec`, or eliminating generic parameters from a
//! type so that it can be used in object-safe trait methods.
//!
//! However, a significant limitation of the `Any` trait is its `'static` lifetime
//! bound, which prevents it from being used for types containing any non-`'static`
//! references. This restriction eliminates many potential use-cases, and in others
//! it can force users to sacrifice performance by cloning data that could otherwise
//! be borrowed.
//!
//! This crate aims to circumvent this limitation through careful use of `unsafe` code
//! hidden behind a safe abstraction, so that type-erasure may be applied to *transient*
//! (i.e. non-`'static`) data. This is achieved by modeling a Rust type as decomposable
//! into separate components for the *raw data* and the *lifetime bounds*, as embodied
//! by the `Static` and `Transience` associated types of the [`Transient`] trait.
//!
//! # Features
//! - Zero run-time cost beyond that of a standard `dyn Any` cast; everything is 
//! implemented using the compile-time type system.
//! - Safely accounts for subtyping and variance.
//! - Supports types with generic type parameters, 0 or more lifetime parameters
//! with arbitrary variances.
//! - Wrappers exhibit the same variance as their inner type and provide methods
//! for performing valid variance conversions.
//! - Provides an `unsafe` public API with narrow safety requirements.
//! 
//! # Limitations
//! - Only emulates the non-`Send`/`Sync` variant of the stdlib's `Any` trait 
//! for now, but support for `Any + Send` and `Any + Send + Sync` can be added 
//! trivially 
//! 
//!
//! # Examples
//!
//! The first step in using this crate is to implement the `Transient` trait
//! for a type. This trait is automatically implemented for many stdlib types,
//! can be derived for most custom types, and is very easy to implement by
//! hand when more flexibility is needed.
//!
//! In the trivial case of a `'static` type with no lifetime parameters, the
//! `transient` crate's `Any` trait can be used just like that of the standard
//! library after deriving the `Transient` trait:
//! ```
//! # fn main() {
//! use transient::*;
//! 
//! #[derive(Transient, Debug, PartialEq, Eq)]
//! struct Usize(usize);
//!
//! let orig = Usize(5);
//!
//! let erased: &dyn Any = &orig;
//! assert_eq!(TypeId::of::<Usize>(), erased.type_id());
//!
//! let restored: &Usize = erased.downcast_ref().unwrap();
//! assert_eq!(restored, &orig);
//! # }
//! ```
//! The trick is that the `Any` trait as used above is actually generic over a
//! type known as the `Transience`, which defaults to `()`; so the relevant line
//! in the above snippet actually desugars to `erased: &'_ dyn Any<()> = &orig`.
//! This form of the `Any` trait only supports `'static` types, just like the
//! stdlib implementation.
//!
//! Where it gets interesting is when a type is *not* `'static`. For such a
//! type, the `Any` trait can be parameterized by a `Transience` type. In
//! the case of a single lifetime parameter, this can simply be one of three
//! types provided by this crate, `Inv`, `Co`, and `Contra`, which represent
//! the three flavors of [variance] a type can have with respect to a lifetime
//! parameter. While choosing the correct variance would typically be a
//! safety-critical decision, the valid choices for the variance of a type
//! are bounded by its implementation of the `Transient` trait, and the compiler 
//! will stop you from choosing incorrectly.
//!
//! We will return to the topic of `Transience` in a bit, but for now lets
//! choose `Inv` (*invariant*) which is the most conservative form of variance
//! that all (single-lifetime) types can use:
//! ```
//! # fn main() {
//! use transient::*;
//! #[derive(Transient, Debug, PartialEq, Eq)]
//! struct UsizeRef<'a>(&'a usize);
//!
//! let five = 5;
//! let orig = UsizeRef(&five);
//!
//! let erased: &dyn Any<Inv> = &orig;
//! assert!(erased.is::<UsizeRef>());
//! assert_eq!(TypeId::of::<UsizeRef>(), erased.type_id());
//!
//! let restored: &UsizeRef = erased.downcast_ref().unwrap();
//! assert_eq!(restored, &orig);
//! # }
//! ```
//!
//!
//!
//!
//! # Overview
//!
//! Whereas the [`std::any::Any`] trait is implemented for all `T: 'static`, the
//! [`transient::Any`] trait is implemented for all [`T: Transient`]. The `Transient`
//! trait is an extremely simple, but `unsafe` trait consisting only of two
//! associated types:
//! ```skip
//! pub unsafe trait Transient {
//!     type Static: 'static;
//!     type Transience: Transience;
//! }
//! ```
//! The first associated type `Static` is referred to as the *static type* of the
//! implementing type, and is simply the same type but with its lifetime parameters
//! replaced by `'static` (e.g., a struct `S<'a, 'b>` would define `Static` as
//! `S<'static, 'static>`). The static type is used to obtain a [`TypeId`] that
//! uniquely identifies the (`'static` version of the) erased type so that it can
//! be safely downcast. However, the compiler only assigns `TypeId`s for `'static`
//! types, so any information about the true lifetime parameters of the `Transient`
//! type is lost. Another mechanism is therefore needed to restore this lifetime
//! information so that the borrow checker can continue to maintain Rust's safety
//! guarantees.
//!
//! The second associated type `Transience` provides this mechanism by capturing
//! the lifetime (and *[variance]*) information that the static type is missing.
//! To accomplish this, the `transient` crate provides the `Co`, `Contra` and
//! `Inv` structs that exhibit the 3 forms of variance for a single lifetime
//! parameter, which can be combined in tuples to accommodate types with multiple
//! (or zero) lifetime parameters. This type plays several key roles in the safety
//! and flexibility of this crate's functionality, as will be discussed below.
//!
//! todo!
//!
//! # Explanation
//! This crate makes heavy use of the [`std::mem::transmute`] function to implement
//! its functionality, which is typically a *very dangerous* operation. There are
//! two main concerns when using this function:
//! - Data layouts: Attempting to transmute between types with incompatible data
//! layouts results in instant UB. The `transient` crate avoids this issue by only
//! ever transmuting between a source type and the *same* type with its lifetime
//! parameters replaced by `'static`. This is sound because the lifetimes parameters
//! of a type *do not* affect its memory representation; in fact, lifetimes are only
//! used for borrow checking in early stages of compilation, after which they are
//! discarded entirely.
//! - Unbounded lifetimes: When a type that contains temporarily-borrowed data has
//! its lifetime parameters transmuted to `'static`, the borrow checker will stop
//! protecting it from being used after the borrowed data it references gets dropped.
//! Safely working with an *unbounded* type like this therefore requires some other
//! mechanism for upholding Rust's borrowing rules, which the `transient` crate
//! provides using a concept it refers to as *transience*. This concept, and how
//! it provides such a safety mechanism, will be discussed further in the following
//! sections.
//!
//! # Transience
//! In common language, **transience** is a noun that can be defined as
//! [*the quality or state of being transient*]. The `transient` crate adopts this
//! term throughout its code and documentation to describe the relationship that a
//! data structure has with the 0 or more lifetimes parameters it depends on. More
//! specifically, this refers the [variance] of a type with respect to each of its
//! generic lifetime parameters. While variance is a fairly niche topic in everyday
//! Rust programming, it has major implications for the soundness of the `unsafe`
//! code used to implement the crate's functionality.
//!
// variance table
//! <table style="width:100%">
//!   <tr style="font-size: 14px;">
//!     <th style="width:1%;"> </th>
//!     <th> <code>Inv&lt'short&gt</code> </th>
//!     <th> <code>Inv&lt'long&gt</code> </th>
//!     <th> <code>Co&lt'short&gt</code> </th>
//!     <th> <code>Co&lt'long&gt</code> </th>
//!     <th> <code>Contra&lt'short&gt</code> </th>
//!     <th> <code>Contra&lt'long&gt</code> </th>
//!     <th> <code>Timeless</code> </th>
//!   </tr>
//!   <tr>
//!     <th style="font-size: 14px;background-color:white"> <code>Inv&lt'short&gt</code> </th>
//!     <td align="center" style="background-color:lightgrey;"> - </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!   </tr>
//!   <tr>
//!     <th style="font-size: 14px;background-color:white"> <code>Inv&lt'long&gt</code> </th>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:lightgrey;"> - </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!   </tr>
//!   <tr>
//!     <th style="font-size: 14px;background-color:white"> <code>Co&lt'short&gt</code> </th>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:lightgrey;"> - </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!   </tr>
//!   <tr>
//!     <th style="font-size: 14px;background-color:white"> <code>Co&lt'long&gt</code> </th>
//!     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
//!     <td align="center" style="background-color:lightgrey;"> - </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!   </tr>
//!   <tr>
//!     <th style="font-size: 14px;background-color:white"> <code>Contra&lt'short&gt</code> </th>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:lightgrey;"> - </td>
//!     <td align="center" style="background-color:#FFD966;"> yes<sup>&lowast;</sup> </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!   </tr>
//!   <tr>
//!     <th style="font-size: 14px;background-color:white"> <code>Contra&lt'long&gt</code> </th>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!     <td align="center" style="background-color:lightgrey;"> - </td>
//!     <td align="center" style="background-color:#FF9B9B;"> no </td>
//!   </tr>
//!   <tr>
//!     <th style="font-size: 14px;background-color:white"> <code>Timeless</code> </th>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:lightgreen;"> yes </td>
//!     <td align="center" style="background-color:lightgrey;"> - </td>
//!   </tr>
//! </table>
//! * transitions marked with an asterisk are allowed, but lossy; the recovered
//! lifetime will be reduced (Co) or increased (Contra).
//!
//! Consider the following struct:
//! ```
//! struct S<'a> {
//!     string: &'a str,
//! }
//! ```
//! By transmuting the lifetime to `'static`, you are telling the borrow checker
//! to see the struct like this instead:
//! ```
//! struct StaticS { // equivalent to `S<'static>`
//!     string: &'static str,
//! }
//! ```
//! From a raw data perspective, these structs are identical; the only difference
//! is in the metadata that the borrow checker uses to evaluate whether your code
//! should be accepted. This makes transmuting between the two types sound, but
//! the transmuted value is still dangerous because there is nothing to prevent
//! it from being used after the borrowed strings are dropped.
//!
//! To address this danger, we can place this unbounded value in a wrapper struct
//! along with some [`std::marker::PhantomData`] bound to the original lifetime:
//! ```
//! # struct S<'a> {string: &'a str}
//! # struct StaticS {string: &'static str}
//! struct SafetyWrapper<'a> {
//!     dangerous: S<'static>,
//!     lifetime: std::marker::PhantomData<&'a ()>
//! }
//! pub fn make_static_and_wrap(s: S<'_>) -> SafetyWrapper<'_> {
//!     SafetyWrapper {
//!         // safe because we know the types have the same layout
//!         dangerous: unsafe { std::mem::transmute::<_, S<'static>>(s) },
//!         lifetime: std::marker::PhantomData
//!     }
//! }
//! pub fn make_transient_and_unwrap<'a>(wrapped: SafetyWrapper<'a>) -> S<'a> {
//!     // safe because we know the types have the same layout
//!     unsafe { std::mem::transmute::<S, S<'a>>(wrapped.dangerous) }
//! }
//! ```
//! While the inner value is *not* bound to `'a`, the wrapper itself *is*, and
//! the borrow checker will protect the entire struct accordingly; and if the
//! wrapper's public API is careful not to expose the unbounded inner value,
//! we get something that starts to smell like a safe abstraction! If only it
//! were that simple...
//!
//! If it weren't for one pesky detail, we would be done here... and that detail
//! involves the complicated concept of [variance]. In summary, *most* types are
//! are *covariant* which allows them to implicitly shorten their lifetimes,
//! such as when passing `&'static str` as an argument expecting `&'short str`.
//! However, some types are *invariant* which disables this behavior, and some
//! types are *contravariant* which completely reverses it (e.g., you can pass
//! `fn(&'short str)` to a function expecting `fn(&'static str)`).
//!
//! While the details are not important, the key takeaway is that types can
//! have various relationships with their lifetime parameter(s); and this
//! becomes critically important to the soundness of the example above.
//!
//! As an example, consider the following struct that is *invariant* with
//! respect to `'a`, and the same `SafetyWrapper` implementation as before:
//! ```
//! struct InvS<'a> {
//!     cell: std::cell::Cell<&'a str>,
//! }
//! struct SafetyWrapper<'a> {
//!     dangerous: InvS<'static>,
//!     lifetime: std::marker::PhantomData<&'a ()>
//! }
//! ```
//! Wrapping this struct as before is *almost* safe, and typical usage will not
//! invoke UB; after all, we keep the unbounded value hidden until its lifetime
//! is restored to `'a`, after which we are back to business as usual. However,
//! this is not actually *sound*, and the following sequence can lead to UB:
//!
//! - We start with `InvS<'long>`, which cannot be shortened to `InvS<'short>`.
//! - We then place it in the wrapper as before to get `SafetyWrapper<'long>`.
//! - We then pass this wrapper to a function requiring `SafetyWrapper<'short>`,
//! which is allowed due to the *covariance* of the wrapper as implemented above.
//! - Inside this function, we restore the wrapped value as `InvS<'short>`.
//!
//! These steps allowed us to "safely" shorten the lifetime of `InvS<'long>`
//! to `InvS<'short>`, but this is supposed to be illegal for invariant types
//! due to UB it can lead to!
//!
//! The conclusion here is that we need the wrapper to exhibit the same variance
//! as the wrapped value, which in our example could look like this:
//! ```
//! struct InvS<'a> {
//!     cell: std::cell::Cell<&'a str>,
//! }
//! struct SafetyWrapper<'a> {
//!     dangerous: InvS<'static>,
//!     lifetime: std::marker::PhantomData<std::cell::Cell<&'a ()>>
//! }
//! ```
//! And that does the trick; as long as we don't touch the unbounded inner value,
//! this is completely safe! Now this might not sound particularly useful; after
//! all, if can't safely do anything with the inner value then what's the point?
//! While it is true that we can't safely interact with the inner value, the true
//! power comes from the ability it gives us to easily reason about the soundness
//! of `unsafe`-ly interacting with it. For example, anytime we have access to the
//! (protected) wrapper we immediately know that the (unprotected) inner value is
//! still valid, and that we can safely interact with it; we just need to be careful
//! not to release the unbounded inner value to safe code where it can been used
//! after our access to the wrapper expires (which ends up being the *only* safety
//! consideration necessary in many cases).
//!
//! For example, we could define the following *sound* public API exposing safe and
//! slightly-`unsafe` methods:
//! ```
//! # use std::{cell::Cell, marker::PhantomData};
//! # struct InvS<'a> { cell: Cell<&'a str> }
//! # struct SafetyWrapper<'a> {dangerous: InvS<'static>, lifetime: PhantomData<Cell<&'a ()>>}
//! impl<'a> SafetyWrapper<'a> {
//!     /// This public method is completely safe.
//!     pub fn print_inner(&self) {
//!         println!("{:?}", &self.dangerous.cell)
//!     }
//!     /// This public method needs to be marked `unsafe` because the returned
//!     /// value has an unbounded lifetime. However, it is not actually *that*
//!     /// unsafe, since the returned reference keeps it bound to the safe
//!     /// wrapper. The only real problem would be if the caller `clone`'s
//!     /// that reference to get an owned and fully unbounded `InvS<'static>`,
//!     /// which we do need to warn against:
//!     ///
//!     /// # Safety
//!     /// Something something, what I said above.
//!     pub unsafe fn get_inner(&self) -> &InvS<'static> {
//!         &self.dangerous
//!     }
//! }
//! ```
//!
//! And there we have it, we have created a sound API for our invariant struct
//! using the same general approach as the `transient` crate! However, we would
//! need to repeat this process by hand for every struct, and we can't use generics
//! for it because there would be no way to predict the variance of the generic
//! type (we *could* just force invariance, but then we'd lose out on the benefits
//! of covariance which would be appropriate for most types). We would also need
//! to account for more complicated cases, such as structs with generic type
//! parameters and/or multiple lifetime parameters.
//!
//! Luckily we have this crate to handle all of that instead, and all we have to
//! do is implement (or derive) a single `unsafe` but straight-forward trait for
//! our struct that provides the necessary information for building the safe API.
//!
//! For our example, it would look like this:
//! ```
//! # struct InvS<'a> { cell: std::cell::Cell<&'a str> }
//! use transient::*;
//! unsafe impl<'a> Transient for InvS<'a> {
//!     type Static = InvS<'static>;
//!     type Transience = Inv<'a>;
//! }
//! ```
//!
//! # Approach
//!
//! The following steps are meant to illustrate what the crate does behind-the-scenes
//! to safely implement its functionality; skip to [the next section](#usage) if you
//! don't care and just want to learn about using it.
//!
//! 1. The [`Transient<'src>`] trait is implemented/derived for a type, which is a
//! simple but `unsafe` trait that allows a transient type (or a reference to such)
//! with minimum lifetime bound `'src` to be transmuted into a `'static` version of
//! the same type. On its own, this operation would be extremely `unsafe`, but the
//! following steps will make use of the trait's `'src` lifetime parameter to build
//! a safe abstraction.
//!
//! 2. The `'static`-ified type is then *erased* by casting to [`dyn Any`] (behind
//! a box or reference), which is now possible thanks to the falsely-`'static`
//! lifetime. However, using this object directly is still dangerous as there is
//! no lifetime bounding access to the borrowed data it contains.
//!
//! 3. The erased value (or shared/mutable reference) is then wrapped in an [`Erased`]
//! (or [`ErasedRef`]/[`ErasedMut`]) struct, which uses [`PhantomData`] to bind the
//! value to its true lifetime `'src` and ensure that the borrowed data remains valid
//! for the lifetime of the wrapper. Furthermore, the API of this wrapper struct is
//! designed such that the wrapped value is *not* exposed in any safe public methods,
//! and cannot be extracted or referenced directly.
//!
//! 4. Finally, each wrapper provides a `restore<T>` method that can be called to
//! extract the value (or reference) in its original form. This method attempts to
//! downcast the erased value as the given type `T`, and then restores the original
//! lifetime `'src` before returning it to the caller.
//!
//! # Usage
//! First, the [`Transient`] trait is implemented for a type which may or may not
//! have generic lifetime or type parameters. This trait is already implemented
//! for many common foreign types, and can be easily implemented for custom types
//! either by hand or using the [`Transient` derive macro].
//!
//! The trait's methods can then be called to safely erase a type by value ([`erase`]),
//! shared reference ([`erase_ref`]), or mutable reference ([`erase_mut`]). These
//! methods return the erased type wrapped in an [`Erased`], [`ErasedRef`], or
//! [`ErasedMut`] struct that maintains safety by binding to the original lifetime
//! and restricting access to the unbounded inner value it wraps.
//!
//! The type-erased wrapper can then be used in dynamically typed patterns like
//! those enabled by the [`Any`] trait; but where `Box<dyn Any>` would be used
//! to erase the type of an owned value with a necessarily `'static` lifetime,
//! for example, you would instead use [`Erased<V>`][`Erased`] and gain the
//! freedom to erase transient types containing non-`'static` references.
//!
//! When dynamic typing is no longer needed, the wrapper's `restore` method
//! ([`Erased::restore`], [`ErasedRef::restore`], or [`ErasedMut::restore`]) can
//! be called to move out of the wrapper and return the inner value/reference with
//! its static type and lifetime bounds restored.
//!
//! # Examples
//!
//! The following code block provides a basic example of using this crate to
//! utilize `Any`-like dynamic typing for a non-`'static` struct. Explicit type
//! annotations will be included for clarity, wherein the anonymous lifetime
//! `'_` will be used to represent the `'src` lifetime.
//!
//! ```skip
//! use transient::{Transient, Inv};
//!
//! #[derive(Transient, Clone, Debug, PartialEq, Eq)]
//! struct S<'a> {
//!     value: &'a String,
//! }
//!
//! // Create a `String` that the `S` struct will borrow. The lifetime of this
//! // string ultimately defines the `'src` lifetime, and all references to it
//! // must be dropped before it goes out of scope.
//! let string = "qwer".to_string();
//!
//! // Create a "transient" struct that borrows the string:
//! let original: S<'_> = S{value: &string};
//!
//! // Extend lifetime, erase type, and wrap with an `Erased` struct to preserve
//! // and enforce its lifetime bound:
//! let erased: Erased<Inv<'_>> = original.erase();
//!
//! let x = 4.ierase();
//!
//! // We can now do dynamically typed things with it, such as storing it in a
//! // `Vec` with other erased types:
//! let _ = vec![
//!     erased, 4.erase().into_transience(),
//!     Box::new(2.0).erase().into_transience()];
//! ```
//!
//! ```compile_fail
//! # use transient::Transient;
//! # #[derive(Transient, Clone, Debug, PartialEq, Eq)]
//! # struct S<'a> {value: &'a String} let string = "qwer".to_string();
//! # let original = S{value: &string}; let erased = original.erase();
//! // Even though the `S` struct's lifetime has been transmuted to `'static`,
//! // the wrapper keeps it bound to the true lifetime and the borrow checker
//! // would reject the next line to protect us from use-after-free:
//! drop(string);
//! # let _ = erased.clone();  // needed to prevent early drop
//! ```
//!
//! ```rust
//! # use transient::{Transient};
//! # #[derive(Transient, Clone, Debug, PartialEq, Eq)]
//! # struct S<'a> {value: &'a String} let string = "qwer".to_string();
//! # let original = S{value: &string}; let erased = original.clone().erase_ref();
//! // Restore the static type and lifetime of the transient struct:
//! let restored: S<'_> = erased.restore().unwrap();
//! assert_eq!(&restored, &original);
//! ```
//!
//! ```compile_fail
//! # use transient::Transient;
//! # #[derive(Transient, Clone, Debug, PartialEq, Eq)]
//! # struct S<'a> {value: &'a String} let string = "qwer".to_string();
//! # let original = S{value: &string}; let erased = original.erase();
//! # let restored: S = erased.restore().unwrap();
//! // Now that the true lifetime of the inner value has been restored, the
//! // borrow checker will continue to protect us by rejecting the next line:
//! drop(string);
//! # let _ = restored.clone();  // needed to prevent early drop
//! ```
//!
//! ```rust
//! # use transient::{Transient};
//! # #[derive(Transient, Clone, Debug, PartialEq, Eq)]
//! # struct S<'a> {value: &'a String} let string = "qwer".to_string();
//! # let original = S{value: &string}; let erased = original.erase();
//! # let restored: S = erased.restore().unwrap();
//! // However, once `restored` gets dropped the borrow is released and we
//! // are free to drop or move the string again!
//! drop(restored);
//! drop(string);
//! ```
//!
//! # Variance
//!
//! Covariance:
//! ```skip
//! use transient::*;
//! fn shrink<'short, 'long: 'short>(long: Erased<Co<'long>>) -> Erased<Co<'short>> {
//!     long
//! }
//! ```
//! Invariance:
//! ```compile_fail
//! use transient::*;
//! fn shrink<'short, 'long: 'short>(long: Erased<Inv<'long>>) -> Erased<Inv<'short>> {
//!     long
//! }
//! ```
//!
//! [`PhantomData`]: std::marker::PhantomData
//! [`dyn Any`]: https://doc.rust-lang.org/std/any/index.html#any-and-typeid
//! [`transient::Any`]: transient::Transient
//! [`Transient`]: transient::Transient
//! [`T: Transient`]: transient::Transient
//[`Transient`]: ../transient/trait.Transient.html
//! [`Transient` derive macro]: transient_derive::Transient
//! [`Transient` trait]: Transient
//! [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
//! [*the quality or state of being transient*]: https://www.merriam-webster.com/dictionary/transience

// #![warn(missing_docs)]

#[cfg(test)]
pub mod tests;

pub mod transience;
mod transient;
mod any;

#[doc(inline)]
pub use crate::transient::Transient;
pub use crate::transient::{StaticWrap, WrapStaticType};

#[doc(inline)]
pub use crate::any::{Any, AnyOps, UnsafeOps, TypeId};

#[doc(inline)]
pub use transience::{
    Transience, Timeless, Co, Contra, Inv,
};


#[cfg(feature = "derive")]
pub use transient_derive::Transient;
