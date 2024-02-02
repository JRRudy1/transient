
use std::{fmt::Debug, marker::PhantomData};

/// Enumerates the available variance types.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VarianceTag {
    Static,
    Invariant,
    Covariant,
    #[allow(unused)]
    Contravariant,
}

/// Sealed unsafe trait for zero-sized types that set the [variance] of a wrapper.
///
/// Note that even though most types are *covariant* in reality, this crate
/// treats *invariance* as the default since any other assumption could cause
/// undefined behavior if chosen incorrectly. To override this default, the
/// [`Variance`] associated type can be set in the type's `TransientAny`
/// implementation; if using the derive macro, this corresponds to including
/// the `#[r#unsafe(covariant)]` attribute.
///
/// # SAFETY
/// Must be a zero-sized-type with the variance suggested by it's name.
///
/// [`Variance`]: crate::TransientAny::Variance
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
/// _private::Sealed +  // todo
pub unsafe trait Variance: Debug {
    const TAG: VarianceTag;
}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a *static* type with no generic lifetime parameters.
///
/// Such types only contain owned data and static references, and are thus much
/// safer to work with and allow several restrictions imposed by the crate to
/// be loosened. For example, the erased wrapper structs typically restrict all
/// access to the inner `dyn Any`, but expose safe public methods for getting
/// accessing it when the wrapped type is `'static`.
pub type Static = PhantomData<()>;
unsafe impl Variance for Static {
    const TAG: VarianceTag = VarianceTag::Static;
}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *invariant* with respect to its lifetime parameter `'a`.
///
/// An *invariant* type is one for which the compiler cannot safely assume that
/// its lifetime may be shortened **or** lengthened (such as `&'a mut T`). Such
/// a type must therefore match the expected lifetime exactly when passed to a
/// function.
///
/// See the [`Variance`] documentation for more information.
pub type Invariant<'a> = PhantomData<fn(&'a ()) -> &'a ()>;
unsafe impl<'a> Variance for Invariant<'a> {
    const TAG: VarianceTag = VarianceTag::Invariant;
}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *covariant* with respect to its lifetime parameter `'a`.
///
/// A *covariant* type is one for which the compiler can safely shorten its
/// lifetime parameter as needed when passing it to a function; for example,
/// `&'a T` is *covariant* w.r.t. `'a`, so a `&'static str` can be passed
/// to a function with signature `f(&'short str)` without issue.
///
/// See the [`Variance`] documentation for more information.
pub type Covariant<'a> = PhantomData<&'a ()>;
unsafe impl<'a> Variance for Covariant<'a> {
    const TAG: VarianceTag = VarianceTag::Covariant;
}

/// Placeholder for future implementation of contravariance. This struct
/// currently does not implement [`Variance`] and cannot be used.
#[allow(unused, dead_code)]
pub type Contravariant<'a> = PhantomData<fn(&'a ())>;


mod _private {
    /// Prevents `Variance` from being implemented in downstream crates.
    #[doc(hidden)] pub trait Sealed {}
    #[doc(hidden)] impl Sealed for super::Static {}
    #[doc(hidden)] impl<'a> Sealed for super::Invariant<'a> {}
    #[doc(hidden)] impl<'a> Sealed for super::Covariant<'a> {}
    #[doc(hidden)] impl<'a> Sealed for super::Contravariant<'a> {}
}
