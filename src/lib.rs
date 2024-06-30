//! This crate provides the [`transient::Any`] trait which re-implements the
//! dynamic typing mechanism provided by [`std::any::Any`] to add support for
//! types with non-`'static` lifetimes.
//!
//! # Introduction
//! The standard library's [`Any`] trait is used to emulate dynamic typing within
//! Rust, and is extremely useful in cases where implementing a statically typed
//! solution would be inconvenient, if not impossible. Examples include storing
//! heterogeneous values in a `Vec`, or eliminating generic parameters from a
//! type so that it can be used in object-safe trait methods.
//!
//! However, a significant limitation of the `std::any::Any` trait is its `'static`
//! lifetime bound, which prevents it from being used for types containing any
//! non-`'static` references. This restriction eliminates many potential use-cases,
//! and in others it can force users to sacrifice performance by cloning data that
//! could otherwise be borrowed.
//!
//! The crate provides a re-implemented [`Any`] trait that circumvents this limitation
//! to allow type-erasure to be applied to *transient* (i.e. non-`'static`) types.
//! This is achieved by modeling a Rust type as decomposable into separate components
//! for its _raw static data_ and its _lifetime parameters_, as embodied by the
//! `Static` and `Transience` associated types of the provided [`Transient`] trait.
//! In this implementation, the `Static` component is used to obtain the unique
//! [`TypeId`] of the type (which the compiler only hands out for `T: 'static`),
//! and the `Transience` is used as a generic parameter on the re-implemented
//! [`Any`] trait to bound the allowable transitions and uphold Rust's strict
//! safety guarantees.
//!
//! # Features
//! - Near drop-in replacement for `std::any::Any` when dealing with `'static` types
//! - Familiar extension beyond `std::any::Any` when dealing with non-`'static` types
//! - Zero run-time cost above that of a standard `dyn Any` cast, with all added
//!   functionality implemented using the type system
//! - Safely accounts for the nuances of _subtyping and variance_
//! - Supports types with any number of generic lifetime parameters with arbitrary
//!   variance combinations
//! - Supports types with any number of generic type parameters
//! - Provides the [`macro@Transient`] `derive` macro to implement the `Transient`
//!   trait for most types
//!
//! # Limitations
//! - Requires a single `unsafe` trait to be implemented for types wishing to
//!   utilize the crate's functionality; however, this trait is usually trivial
//!   to safely implement, and a `derive` macro is provided for common cases
//! - Only `Sized` types are supported. Removing this restriction would be
//!   trivial, but makes it awkward to name generic types that require their
//!   parameters to be `T: Sized` since `T::Static: Sized` must be explicitly
//!   stated even when `T: Sized` can be implied
//!
//! # Examples
//!
//! The first step in using this crate is to implement the [`Transient`] trait
//! for a type. Implementations of this trait are provided for many stdlib
//! types, it can be derived for most custom types, and it is easy to implement
//! by hand when more flexibility is needed. Implementations for common types
//! provided by some 3rd party libraries are also available behind eponymous
//! feature flags (currently only `ndarray`, `pyo3`, and `numpy`, but feel free
//! to submit an issue/PR requesting others).
//!
//! In the trivial case of a `'static` type with no lifetime parameters, the
//! `transient` crate's [`Any`] trait can be used just like that of the standard
//! library once the `Transient` trait has been implemented or derived:
//! ```
//! # fn main() {
//! use transient::*;
//!
//! #[derive(Transient, Debug, PartialEq)]
//! struct Usize(usize);
//!
//! let orig = Usize(5);
//!
//! let erased: &dyn Any = &orig;
//! assert_eq!(TypeId::of::<Usize>(), erased.type_id());
//!
//! let restored: &Usize = erased.downcast_ref::<Usize>().unwrap();
//! assert_eq!(restored, &orig);
//! # }
//! ```
//! The trick is that the `Any` trait as used above is actually generic over a
//! type known as the `Transience`, which defaults to `()`; so the relevant line
//! in the above snippet actually desugars to `erased: &'_ dyn Any<()> = &orig`.
//! This form of the `Any` trait only supports `'static` types, just like the
//! stdlib implementation.
//!
//! Where it gets interesting is when a type is *not* `'static`, for which the
//! `Any` trait can be parameterized by a [`Transience`] type. In the case of
//! a type with a single lifetime parameter, this can simply be one of three types
//! provided by this crate, [`Inv`], [`Co`], and [`Contra`], which represent the
//! three flavors of [variance] a type can have with respect to a lifetime parameter.
//! While choosing the correct variance would typically be a safety-critical
//! decision, the valid choices for the variance of a type are bounded by its
//! implementation of the `Transient` trait, and the compiler will prevent you
//! from using a transience that would not be sound.
//!
//! We will return to the topic of `Transience` in a bit, but for now lets choose
//! `Inv` (*invariant*) which is the most conservative form of variance that all
//! (single-lifetime) types can use. To do this, simply replace `dyn Any` with
//! `dyn Any<Inv>` when coercing a `Box` or reference to the trait object:
//! ```
//! # fn main() {
//! use transient::*;
//!
//! #[derive(Transient, Debug, PartialEq)]
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
//! And that's all it takes! Things get a slightly spicier in more complicated
//! scenarios, but this crate aims to make the process as painless and intuitive
//! as possible while safely providing a high degree of flexibility for all the
//! niche cases you can imagine.
//!
//!
//! # Overview
//!
//! ## The `Any` trait
//! The most important item provided by this crate is the [`Any`] trait, which is
//! modeled after the standard library's [`std::any::Any`] trait. Much like the
//! stdlib version, this trait typically appears as the opaque `dyn Any` trait
//! object that can be _downcast_ back into an original concrete type. The key
//! difference is that, while the `std::any::Any` trait is implemented for all
//! `T: 'static`, the [`transient::Any`] trait is instead implemented for all
//! `T: Transient` (as discussed in the next section). The `transient::Any`
//! trait is also different in that it has a generic type parameter know as
//! he `Transience` (discussed in another upcoming section) which is used to
//! enable the support for non-`'static` types that forms the motivation for
//! this crate.
//!
//! ## The `Transient` Trait
//! The [`Transient`] trait is an extremely simple, but `unsafe` trait consisting
//! only of two associated types:
//! ```skip
//! pub unsafe trait Transient {
//!     type Static: 'static;
//!     type Transience: Transience;
//!     /* provided methods hidden */
//! }
//! ```
//! The first associated type `Static` is referred to as the *static type* of the
//! implementing type, and is simply the same type but with its lifetime parameters
//! replaced by `'static` (e.g., a struct `S<'a, 'b>` would define `Static` as
//! `S<'static, 'static>`). The static type is used to obtain a [`TypeId`] that
//! uniquely identifies the (`'static` version of the) erased type so that it can
//! be safely downcast from an opaque trait object to the concrete type. However,
//! the compiler only assigns `TypeId`s for `'static` types, so any information
//! about the true lifetime parameters of the `Transient` type is lost. Another
//! mechanism is therefore needed to restore this lifetime information so that
//! the borrow checker can continue to maintain Rust's safety guarantees.
//!
//! The second associated type `Transience` provides this mechanism by capturing
//! the lifetime (and *[variance]*) information that the static type is missing.
//! To accomplish this, the `transient` crate provides the `Co`, `Contra` and `Inv`
//! structs that exhibit the 3 forms of variance for a single lifetime parameter,
//! which can be then combined in tuples to accommodate types with multiple (or
//! zero) lifetime parameters. This type plays several key roles in the safety
//! and flexibility of this crate's functionality, as will be discussed below.
//!
//! Implementing this trait for a type, either manually or by using the included
//! [derive macro][macro@Transient], is the key ingredient to utilizing the
//! functionality of this crate and is discussed in-depth in
//! [its documentation][Transient].
//!
//! ## The `Transience` trait
//! In common language, **transience** is a noun that can be defined as
//! [*the quality or state of being transient*]. The `transient` crate adopts this
//! term throughout its code and documentation to describe the relationship that a
//! data structure has with the 0 or more lifetimes parameters it depends on, as
//! codified by the [`Transience`] trait. More specifically, this therm refers the
//! [variance] of a type with respect to each of its generic lifetime parameters,
//! which is a fairly niche topic in everyday Rust programming by plays a major
//! role in the implementation of this crate.
//!
//! ### Transience bounds and transitions
//! A simplified version of this crate's functionality could be implemented by
//! simply allowing a type `T: Transient` to be cast to, and restored from, a
//! `dyn Any<T::Transient>` trait object. This would be sufficient in some cases,
//! but is has a significant limitation in that two types `S` and `T` with
//! differing `Transience` types would erase to different trait objects; the
//! erased types `dyn Any<S::Transience>` and `dyn Any<T::Transience>` would
//! be distinct types that could not be used interchangeably or stored together
//! in a homogeneous container.
//!
//! To evade this limitation and provide maximum flexibility, the `transient::Any`
//! trait has a bounded blanket implementation that allows a type to erase to
//! any transience which is more (or equally) conservative than its own. For
//! example, a type `struct S<'long>(&'long i32)` that implements `Transient`
//! with a `Transience` of `Co<'long>` can be erased to `dyn Any<_>` with any
//! compatible transience such as `Co<'long>`, `Co<'short>`, `Inv<'long>`, and
//! `Inv<'short>`.
//!
//! #### Mixing _covariant_ and _contravariant_ types
//! As a result of the flexibility discussed above, the following example of
//! storing covariant and contravariant types in the same `Vec` is possible:
//! ```
//! use transient::*;
//!
//! struct CoStruct<'a>(&'a i32);
//! unsafe impl<'a> Transient for CoStruct<'a> {
//!     type Static = CoStruct<'static>;
//!     type Transience = Co<'a>;
//! }
//!
//! struct ContraStruct<'a>(fn(&'a i32));
//! unsafe impl<'a> Transient for ContraStruct<'a> {
//!     type Static = ContraStruct<'static>;
//!     type Transience = Contra<'a>;
//! }
//!
//! let value: i32 = 5;
//! fn func(val: &i32) { dbg!(val); }
//!
//! // `co` could erase to `dyn Any<Co>`, but also `dyn Any<Inv>`
//! let co = Box::new(CoStruct(&value));
//! // `co` could erase to `dyn Any<Contra>`, but also `dyn Any<Inv>`
//! let contra = Box::new(ContraStruct(func));
//! // the type annotation coerces both to choose the latter
//! let erased_vec: Vec<Box<dyn Any<Inv>>> = vec![co, contra];
//!
//! assert!(erased_vec[0].downcast_ref::<CoStruct>().is_some());
//! assert!(erased_vec[1].downcast_ref::<ContraStruct>().is_some());
//! ```
//! Note however, that this technique is not _always_ possible; if you have a
//! `CoStruct<'short>` and `ContraStruct<'long>`, there would be no common
//! `Transience` for them to erase to; the first _cannot_ be lengthened to the
//! `'long` lifetime due to its covariance, and the second _cannot_ be shortened
//! to `'short` due to its contravariance.
//!
//! #### Mixing types with different numbers of lifetime parameters
//! Type with more than one lifetime parameter can use a tuple containing a
//! variance item for each lifetime as their `Transience`; this is discussed
//! in-depth in the documentation for the [`Transience`] trait. Consider the
//! following example defining two types, the first with a single lifetime
//! and the second with two. We will choose _invariance_ for all lifetime
//! parameters for simplicity, but when it comes to usage, a similar
//! situation to the "mixed co- and contra-variance " example above arises;
//! if we want to use the types together, we need to find a way to erase
//! both types to a common `dyn Any<_>` trait object:
//! ```
//! use transient::*;
//!
//! struct OneLifetime<'a>(&'a i32);
//! unsafe impl<'a> Transient for OneLifetime<'a> {
//!     type Static = OneLifetime<'static>;
//!     type Transience = Inv<'a>;
//! }
//!
//! struct TwoLifetimes<'a, 'b>(&'a i32, &'b i32);
//! unsafe impl<'a, 'b> Transient for TwoLifetimes<'a, 'b> {
//!     type Static = TwoLifetimes<'static, 'static>;
//!     // we use a tuple for the `Transience` that covers both lifetimes
//!     type Transience = (Inv<'a>, Inv<'b>);
//! }
//!
//! let (value1, value2) = (5, 7);
//! // The "natural" choice would be erasing to `dyn Any<Inv>`
//! let one = Box::new(OneLifetime(&value1));
//! // The "natural" choice would be erasing to `dyn Any<(Inv, Inv)>`
//! let two = Box::new(TwoLifetimes(&value1, &value2));
//! // The trait objects would not be compatible, but `one` can actually erase
//! // to `dyn Any<(Inv, Inv)>` as well, since adding additional components is
//! // allowed; so let's do that:
//! let erased_vec: Vec<Box<dyn Any<(Inv, Inv)>>> = vec![one, two];
//! assert!(erased_vec[0].downcast_ref::<OneLifetime>().is_some());
//! assert!(erased_vec[1].downcast_ref::<TwoLifetimes>().is_some());
//! ```
//!
//! In this example, we actually could have taken the opposite approach instead
//! by coercing the types to `dyn Any<Inv<'a>>`, which would implicitly force `'a`
//! and `'b` to be equal. In this case that would work since 'a and 'b are indeed
//! equal:
//! ```
//! # use transient::*;
//! # struct OneLifetime<'a>(&'a i32);  
//! # unsafe impl<'a> Transient for OneLifetime<'a> {
//! #     type Static = OneLifetime<'static>;
//! #     type Transience = Inv<'a>;
//! # }
//! # struct TwoLifetimes<'a, 'b>(&'a i32, &'b i32);
//! # unsafe impl<'a, 'b> Transient for TwoLifetimes<'a, 'b> {
//! #     type Static = TwoLifetimes<'static, 'static>;
//! #     type Transience = (Inv<'a>, Inv<'b>);
//! # }
//! let (value1, value2) = (5, 7);
//! let one = Box::new(OneLifetime(&value1));
//! let two = Box::new(TwoLifetimes(&value1, &value2));
//! // allowed because 'a == 'b
//! let erased_vec: Vec<Box<dyn Any<Inv>>> = vec![one, two];
//! assert!(erased_vec[0].downcast_ref::<OneLifetime>().is_some());
//! assert!(erased_vec[1].downcast_ref::<TwoLifetimes>().is_some());
//! ```
//! However if `'a `was `'short` and `'b` was `'long` then the invariance would
//! prevent them from being unified.
//!
//! [`TypeId`]: TypeId
//! [`transient::Any`]: Any
//! [`Transient`]: tr::Transient
//! [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
//! [_subtyping and variance_]: https://doc.rust-lang.org/nomicon/subtyping.html
//! [*the quality or state of being transient*]: https://www.merriam-webster.com/dictionary/transience
#![deny(missing_docs, clippy::missing_safety_doc)]

pub mod any;
pub mod transience;

// intentionally non-public to avoid naming conflicts with the crate
mod transient;

#[doc(inline)]
pub use crate::any::{Any, Downcast, TypeId};

#[doc(inline)]
pub use crate::transient::{Static, Transient};

#[doc(inline)]
pub use transience::{Co, Contra, Inv, Timeless, Transience};

#[doc(inline)]
pub use transience::{Covariant, Contravariant, Invariant, Lifetime};

pub use transience::{CanRecoverFrom, CanTranscendTo};

#[cfg(feature = "derive")]
pub use transient_derive::Transient;

/// Re-exports the [`Transient`] trait to enable unambiguously importing it
/// instead of the [`Transient`][transient_derive::Transient] derive macro.
pub mod tr {
    pub use super::transient::{Static, Transient};
}

#[cfg(test)]
pub mod tests;

#[cfg(doctest)]
#[doc = include_str!("../README.md")]
struct ReadMe;
