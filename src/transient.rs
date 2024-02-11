//! Defines the [`Transient`] trait.
use crate::{
    transience::Transience,
};


pub trait Static: Transient<Static=Self> + Sized + 'static {}

impl<T: ?Sized> Static for T
where
    T: Transient<Static=Self> + 'static
{}

/// Unsafe trait for converting the lifetime parameters of a type to (and from)
/// `'static` so that it can be cast to `dyn Any`. This trait can be derived
/// using the [`Transient` derive macro].
///
/// The most important methods defined by this trait are [`erase`], [`erase_ref`],
/// and [`erase_mut`]; these methods artificially extend the lifetime of a value
/// (or shared/mutable reference) to `'static`, erase its type by casting it to
/// [`dyn Any`], and then place it in an [`Erased`], [`ErasedRef`], or [`ErasedMut`]
/// wrapper struct that provides a safe interface for inspecting and restoring it.
/// Each wrapper struct upholds Rust's safety guarantees by binding to the type's
/// original lifetime `'src`, and restricting safe access to it until the `restore`
/// method ([`Erased::restore`], [`ErasedRef::restore`], or  [`ErasedMut::restore`])
/// is called to downcast the type and restore its original lifetime.
///
/// This trait is marked as `unsafe` because it's lifetime parameter `'src` and
/// associated type `Static` must be defined correctly for the functionality in
/// this crate to be sound. The included [`Transient` derive macro] can be
/// used to correctly implement this trait for any struct with at most **1**
/// lifetime parameter (and any number of type parameters), and for structs with
/// multiple lifetime parameters it is trivial to implement by hand (just be
/// sure to carefully review the [#safety] section to ensure that the required
/// invariants are upheld, and see the [#examples] section for further guidance).
///
/// # SAFETY
/// - The [`Static`][Self::Static] associated type should be the same as the
/// `Self` type but with all lifetime parameters replaced by `'static`.
/// - The [`Transience`][Self::Transience] associated type must either correspond
/// to the true variance of the type with respect to `'src`, or be set to
/// `Invariant` as a safe default. Many types can use `Covariant` instead for
/// increased flexibility, but not all! See [Subtyping and Variance].
/// - The trait's lifetime parameter `'src` must match the *minimum* lifetime
/// parameter declared for the `impl` block, and sufficient bounds must be
/// placed on the lifetime parameters to make this choice unambiguous.
///
/// # Examples
/// Note: The following examples demonstrate how to correctly implement this trait.
/// For practical usage examples, see the [crate documentation][crate#examples].
/// ***
///
/// The simplest case of implementing this trait is for a struct that is already
/// `'static` (i.e. it only contains owned data and/or `'static` references. For
/// such a struct, the lifetime parameter on the trait can also be `'static` and
/// the `Static` associated type can just be `Self`. Of course, this crate would
/// not be necessary in this case (and it is not worth showing a concrete example),
/// but it is still worth mentioning that `'static` types are indeed supported.
///
/// The next simplest case would be a struct with a single lifetime parameter
/// and no generic type parameters:
/// ```
/// use transient::{Transient, Invariant};
/// struct S<'a> {
///     value: &'a str,
/// }
/// // This could also be derived
/// unsafe impl<'a> Transient for S<'a> {
///     type Static = S<'static>;
///     type Transience = Invariant<'a>;
/// }
/// ```
///
/// Generic type parameters are also supported; the only difference for
/// implementing such a type is that the borrow checker will require that
/// all of the type parameters on the impl block have a `'static` lifetime
/// bound. Of course, the generics types themselves may still be borrowed
/// with non-`'static` lifetimes, which is the whole point of this crate.
/// ```
/// use transient::{Transient, Invariant};
/// struct S<'a, T> {
///     value: &'a T,
/// }
/// // This could also be derived. Note that this impl does not apply when
/// // `T` itself is a transient type such as `&'b str`
/// unsafe impl<'a, T: 'static> Transient for S<'a, T> {
///     type Static = S<'static, T>;
///     type Transience = Invariant<'a>;
/// }
/// ```
///
/// Now consider a struct that borrows 2 string slices with independent
/// lifetime parameters (which is currently not supported by the derive
/// macro):
/// ```
/// struct TwoRefs<'a, 'b> {
///     a: &'a str,
///     b: &'b str,
/// }
/// ```
/// For the `Transient` implementation to be sound, a sufficient relationship
/// between the lifetime parameters must be declared on the `impl` block so that
/// a *minimum lifetime* can be unambiguously chosen to parameterize the trait.
///
/// There are three acceptable choices for the lifetime relationships in the
/// `TwoRefs` struct declared above:
/// ```
/// # use transient::{Transient, Invariant};
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// // 'b outlives 'a -> choose 'a for the trait
/// unsafe impl<'a, 'b: 'a> Transient for TwoRefs<'a, 'b> {
///     type Static = TwoRefs<'static, 'static>;
///     type Transience = Invariant<'a>;
/// }
/// ```
/// ```
/// # use transient::{Transient, Invariant};
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// // 'a outlives 'b -> choose `b` for the trait
/// unsafe impl<'b, 'a: 'b> Transient for TwoRefs<'a, 'b> {
///     type Static = TwoRefs<'static, 'static>;
///     type Transience = Invariant<'b>;
/// }
/// ```
/// ```
/// # use transient::{Transient, Invariant};
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// // 'a and 'b are equal -> no need to choose
/// unsafe impl<'a> Transient for TwoRefs<'a, 'a> {
///     type Static = TwoRefs<'static, 'static>;
///     type Transience = Invariant<'a>;
/// }
/// ```
/// However, choosing either `'a` **or** `'b` for the trait without declaring
/// bounds to justify the decision is *unsound* and may lead to undefined
/// behaviour.
///
/// [`dyn Any`]: https://doc.rust-lang.org/std/any/index.html#any-and-typeid
/// [`Erased`]: Erased
/// [`ErasedRef`]: ErasedRef
/// [`ErasedMut`]: ErasedMut
/// [`erase`]: Transient::erase
/// [`erase_ref`]: Transient::erase_ref
/// [`erase_mut`]: Transient::erase_mut
/// [`Transient` derive macro]: transient_derive::Transient
/// [Subtyping and Variance]: https://doc.rust-lang.org/nomicon/subtyping.html
pub unsafe trait Transient: Sized {

    /// Same as `Self` but with all lifetime parameters replaced by `'static`.
    type Static: Static;

    /// todo
    type Transience: Transience;

    /// Get the [TypeId][`std::any::TypeId`] of the `'static`-ified type.
    fn static_type_id() -> std::any::TypeId {
        std::any::TypeId::of::<Self::Static>()
    }
}
