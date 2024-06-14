//! Analogue to the [`std::any`] module, containing re-implementations of
//! [`Any`] and [`TypeId`] that support non-`'static` types alongside
//! re-exports of [`type_name`] and [`type_name_of_val`].
use crate::{
    transience::{CanRecoverFrom, CanTranscendTo, Transience},
    transient::Transient,
};

/// Re-export from the [`std::any`] module.
///
pub use std::any::{type_name, type_name_of_val};

///////////////////////////////////////////////////////////////////////////////
// `Any` trait
///////////////////////////////////////////////////////////////////////////////

/// A trait to emulate dynamic typing, modeled after the [`std::any::Any`] trait with
/// added support for non-`'static` types.
///
/// This trait is primarily used as the `dyn Any<R>` trait object, which has its
/// methods defined on the [`Downcast`] and [`Transcend`] extension traits.
///
/// # Differences from `std::any::Any`
/// - Types must first implement (or derive) the [`Transient`] trait before the
/// blanket impl for all `T: Transient` will apply to them.
/// - In addition to importing the `Any` trait, the [`Downcast`] trait must also
/// be brought into scope for the `dyn Any` methods to become available. An
/// additional extension trait [`Transcend`] can also be imported to add methods
/// that enable transitions from one [`Transience`] to another.
/// - Non-`'static` types can be erased by parameterizing the trait with the
/// desired [`Transience`], which the compiler will ensure is compatible. Types
/// that *are* `'static` can use any `Transience` they want, or exclude the
/// parameter to use the default of `()`.
/// - Not all `dyn Any<_>`'s are equal; the type parameter is considered to be
/// be a part of the type, so `dyn Any<Co<'_>>` is a different type than
/// `dyn Any<()>` and they could not be stored together in the same `Vec`. To
/// circumvent this limitation, a type `T` can be erased to any transience that
/// is a *supertype* of `T::Transience`; for example, a `usize` can be erased
/// to `dyn Any<Co<'_>>` instead of the default `dyn Any<()>` so that it can
/// be stored in a `Vec` with covariant types such as `&'a usize`. Alternatively,
/// methods such as [`transcend`][Transcend::transcend] can be used to adjust the
/// transience at a later time. Note that if the transience is upcast to a
/// shorter lifetime (or a longer lifetime in the *contravariant* case), then
/// it can only be safely [`downcast`][Downcast::downcast] to the shortened lifetime
/// instead of the original (but if you are brave and/or careful, `unsafe`
/// methods such as [`transcend_unbounded`][Transcend::transcend_unbounded]
/// can be used to get around this).
/// - The [`Any::type_id`] method is difficult to use on concrete types as
/// explained in its docstring; using [`TypeId::of_val`] instead.
/// - The `*_unchecked` methods do not require nightly builds.
/// - Only `Box`s using the `Global` allocator are supported.
/// - Only `Sized` types are supported.
///
/// This trait has a blanket `impl` for all [`Transient`] types with a compatible
/// [`Transience`], and cannot be implemented directly.
pub trait Any<R: Transience = ()> {
    /// Gets the `TypeId` of `self`, typically as an erased `dyn Any` trait object.
    ///
    /// Note that this method is much harder to use on a concrete type than the
    /// `std::any::Any::type_id` method, since the trait's generic parameter and
    /// blanket implementation means that any concrete type `T` actually has an
    /// entire family of `Any` implementations (one for each `Transience` type
    /// it can use to fill the `R` parameter), and the specific implementation
    /// to use would need to be specified explicitly using the fully qualified
    /// path (e.g. `<T as Any<Co<'static>>>::type_id(&value)`) even though they
    /// would all give the same result. To avoid this issue, you can instead use
    /// the [`TypeId::of_val`] function (e.g. `TypeId::of_val(&value)`) or the
    /// `Transient::static_type_id` method (e.g. `value.static_type_id()`)
    fn type_id(&self) -> TypeId;
}

impl<T: Transient, R: Transience> Any<R> for T
where
    T::Transience: CanTranscendTo<R>,
{
    #[inline]
    fn type_id(&self) -> TypeId {
        TypeId::of::<T>()
    }
}

impl<R: Transience> std::fmt::Debug for dyn Any<R> + '_ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Any").finish_non_exhaustive()
    }
}

///////////////////////////////////////////////////////////////////////////////
// `dyn Any` extension traits
///////////////////////////////////////////////////////////////////////////////

/// Extension trait defining methods for downcasting the [`dyn Any<_>`][Any] trait
/// object back into a concrete type.
///
/// This trait has an implementation provided for the `dyn Any` trait object,
/// as in not intended to be implemented by downstream types.
pub trait Downcast<R: Transience>: Transcend<R> {
    /// Returns `true` if the concrete type of the erased object is `T`, which can
    /// be used to predict the outcome of calling the [`downcast`][Self::downcast]
    /// and similar methods.
    ///
    /// Slight caveat: this method is _not actually_ comparing the erased type
    /// (call it `E`) to the given type `T`; in reality, it is comparing
    /// `E::Static` to `T::Static` as defined in their [`Transient`] impls. This
    /// is effectively equivalent for most purposes, but see the [`TypeId::of`]
    /// documentation for a discussion of the subtle differences (especially
    /// when using this check in the implementation of `unsafe` code).
    fn is<T: Transient>(&self) -> bool;

    /// Attempt to downcast the box to a concrete type with its lifetime
    /// parameters restored, returning the original in the `Err` variant
    /// if the type was incorrect.
    fn downcast<T: Transient>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
    where
        T::Transience: CanRecoverFrom<R>;

    /// Returns a reference to the inner value with its lifetime parameters
    /// restored if it is of type `T`, or `None` if it isn't.
    fn downcast_ref<T: Transient>(&self) -> Option<&T>
    where
        T::Transience: CanRecoverFrom<R>;

    /// Returns a mutable reference to the inner value with its lifetime
    /// parameters restored if it is of type `T`, or `None` if it isn't.
    fn downcast_mut<T: Transient>(&mut self) -> Option<&mut T>
    where
        T::Transience: CanRecoverFrom<R>;

    /// Downcasts the box to a concrete type without compile-time checks.
    ///
    /// For a safe alternative see [`downcast`][Downcast::downcast].
    ///
    /// # Safety
    /// The contained value must be of type `T::Static`; calling this method with
    /// the incorrect type is *undefined behavior*. However, the the caller is _not_
    /// expected to uphold any lifetime guarantees, since the trait bounds handle
    /// this statically.
    unsafe fn downcast_unchecked<T: Transient>(self: Box<Self>) -> Box<T>
    where
        T::Transience: CanRecoverFrom<R>;

    /// Downcasts the shared reference to a concrete type without runtime checks.
    ///
    /// For a safe alternative see [`downcast_ref`][Downcast::downcast_ref].
    ///
    /// # Safety
    /// The contained value must be of type `T::Static`; calling this method with
    /// the incorrect type is *undefined behavior*. However, the the caller is _not_
    /// expected to uphold any lifetime guarantees, since the trait bounds handle
    /// this statically.
    unsafe fn downcast_ref_unchecked<T: Transient>(&self) -> &T
    where
        T::Transience: CanRecoverFrom<R>;

    /// Downcasts the mutable reference to a concrete type without runtime checks.
    ///
    /// For a safe alternative see [`downcast_mut`][Downcast::downcast_mut].
    ///
    /// # Safety
    /// The contained value must be of type `T::Static`; calling this method with
    /// the incorrect type is *undefined behavior*. However, the the caller is _not_
    /// expected to uphold any lifetime guarantees, since the trait bounds handle
    /// this statically.
    unsafe fn downcast_mut_unchecked<T: Transient>(&mut self) -> &mut T
    where
        T::Transience: CanRecoverFrom<R>;
}

/// Extension trait defining methods for transitioning a [`dyn Any<_>`][Any]
/// trait from one [`Transience`] to another.
///
/// This trait has an implementation provided for the `dyn Any` trait object,
/// as in not intended to be implemented by downstream types.
pub trait Transcend<R: Transience> {
    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend<R2: Transience>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R: CanTranscendTo<R2>;

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend_ref<R2: Transience>(&self) -> &dyn Any<R2>
    where
        R: CanTranscendTo<R2>;

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend_mut<R2: Transience>(&mut self) -> &mut dyn Any<R2>
    where
        R: CanTranscendTo<R2>;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// For a safe alternative see [`transcend`].
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`. Calling this
    /// method with an incompatible transience can lead to *undefined behavior*.
    ///
    /// [`transcend`]: Transcend::transcend
    unsafe fn transcend_unbounded<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R2: Transience;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// For a safe alternative see [`transcend_ref`].
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`. Calling this
    /// method with an incompatible transience can lead to *undefined behavior*.
    ///
    /// [`transcend_ref`]: Transcend::transcend_ref
    unsafe fn transcend_ref_unbounded<R2>(&self) -> &dyn Any<R2>
    where
        R2: Transience;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// For a safe alternative see [`transcend_mut`].
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`. Calling this
    /// method with an incompatible transience can lead to *undefined behavior*.
    ///
    /// [`transcend_mut`]: Transcend::transcend_mut
    unsafe fn transcend_mut_unbounded<R2>(&mut self) -> &mut dyn Any<R2>
    where
        R2: Transience;
}

impl<R: Transience> Downcast<R> for dyn Any<R> + '_ {
    #[inline]
    fn is<T: Transient>(&self) -> bool {
        self.type_id() == TypeId::of::<T>()
    }

    #[inline]
    fn downcast<T: Transient>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
    where
        T::Transience: CanRecoverFrom<R>,
    {
        if self.is::<T>() {
            // We just confirmed that the type is correct.
            Ok(unsafe { self.downcast_unchecked() })
        } else {
            Err(self)
        }
    }

    #[inline]
    fn downcast_ref<T: Transient>(&self) -> Option<&T>
    where
        T::Transience: CanRecoverFrom<R>,
    {
        if self.is::<T>() {
            // We just confirmed that the type is correct.
            Some(unsafe { self.downcast_ref_unchecked() })
        } else {
            None
        }
    }

    #[inline]
    fn downcast_mut<T: Transient>(&mut self) -> Option<&mut T>
    where
        T::Transience: CanRecoverFrom<R>,
    {
        if self.is::<T>() {
            // We just confirmed that the type is correct.
            Some(unsafe { self.downcast_mut_unchecked() })
        } else {
            None
        }
    }

    #[inline]
    unsafe fn downcast_unchecked<T: Transient>(self: Box<Self>) -> Box<T>
    where
        T::Transience: CanRecoverFrom<R>,
    {
        // The caller is expected to ensure that the inner type is `T::Static`,
        // which the `Transient` trait guarantees has the same layout as `T`,
        // so the pointer cast is safe. The trait bound on `T::Transience`
        // ensures that the lifetime parameters of the returned type satisfy
        // the necessary subtyping relationships.
        Box::from_raw(Box::into_raw(self).cast())
    }

    #[inline]
    unsafe fn downcast_ref_unchecked<T: Transient>(&self) -> &T
    where
        T::Transience: CanRecoverFrom<R>,
    {
        // The caller is expected to ensure that the inner type is `T::Static`,
        // which the `Transient` trait guarantees has the same layout as `T`,
        // so the pointer casts are safe. The trait bound on `T::Transience`
        // ensures that the lifetime parameters of the returned type satisfy
        // the necessary subtyping relationships.
        &*(self as *const Self).cast()
    }

    #[inline]
    unsafe fn downcast_mut_unchecked<T: Transient>(&mut self) -> &mut T
    where
        T::Transience: CanRecoverFrom<R>,
    {
        // The caller is expected to ensure that the inner type is `T::Static`,
        // which the `Transient` trait guarantees has the same layout as `T`,
        // so the pointer casts are safe. The trait bound on `T::Transience`
        // ensures that the lifetime parameters of the returned type satisfy
        // the necessary subtyping relationships.
        &mut *(self as *mut Self).cast()
    }
}

impl<R: Transience> Transcend<R> for dyn Any<R> + '_ {
    #[inline]
    fn transcend<R2: Transience>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R: CanTranscendTo<R2>,
    {
        // The trait bound on `R` guarantees that the transience is compatible.
        unsafe { self.transcend_unbounded::<R2>() }
    }

    #[inline]
    fn transcend_ref<R2: Transience>(&self) -> &dyn Any<R2>
    where
        R: CanTranscendTo<R2>,
    {
        // The trait bound on `R` guarantees that the transience is compatible.
        unsafe { self.transcend_ref_unbounded::<R2>() }
    }

    #[inline]
    fn transcend_mut<R2: Transience>(&mut self) -> &mut dyn Any<R2>
    where
        R: CanTranscendTo<R2>,
    {
        // The trait bound on `R` guarantees that the transience is compatible.
        unsafe { self.transcend_mut_unbounded::<R2>() }
    }

    #[inline]
    unsafe fn transcend_unbounded<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R2: Transience,
    {
        // `Box<dyn Any<_>>` always has the same layout regardless of the
        // transience so transmuting the type is safe, and the caller is
        // expected to ensure that the transience is compatible.
        std::mem::transmute(self)
    }

    #[inline]
    unsafe fn transcend_ref_unbounded<R2>(&self) -> &dyn Any<R2>
    where
        R2: Transience,
    {
        // `&dyn Any<_>` always has the same layout regardless of the
        // transience so transmuting the type is safe, and the caller is
        // expected to ensure that the transience is compatible.
        &*(self as *const Self as *const _)
    }

    #[inline]
    unsafe fn transcend_mut_unbounded<R2>(&mut self) -> &mut dyn Any<R2>
    where
        R2: Transience,
    {
        // `&mut dyn Any<_>` always has the same layout regardless of the
        // transience so transmuting the type is safe, and the caller is
        // expected to ensure that the transience is compatible.
        &mut *(self as *mut Self as *mut _)
    }
}

///////////////////////////////////////////////////////////////////////////////
// `TypeID` and its methods
///////////////////////////////////////////////////////////////////////////////

/// Thin wrapper for [`std::any::TypeId`], which represents a globally unique
/// identifier for a type.
///
/// Each `TypeId` is an opaque object which does not allow inspection of what's
/// inside but does allow basic operations such as cloning, comparison,
/// printing, and showing.
///
/// While the `std::any::TypeId` type is currently only available for `'static`
/// types, this wrapped version is instead provided for any type implementing
/// the [`Transient`] trait defined in this crate by simply querying the `TypeId`
/// of the `Static` associated type defined in its `Transient` impl.
///  
/// A slight caveat of this implementation is that this `TypeId` for some type
/// `T: Transient` is _technically_ the unique identifier for [`T::Static`] as
/// defined in its `Transient` impl instead of `T` itself, but as long as the
/// `Transient` trait was implemented correctly (which the `unsafe` implementor
/// pinky-promised they did), then this "static identity" is effectively equivalent.
/// However, this identifier ignores all lifetime information about the type,
/// `&'short str` will have the same `TypeId` as `&'static str`, and `unsafe`
/// code **should not** assume that it can ignore lifetimes based on the `TypeId`
/// alone.
///
/// Quoting from the `std::any::TypeId` documentation: while `TypeId` implements
/// `Hash`, `PartialOrd`, and `Ord`, it is worth noting that the hashes and ordering
/// will vary between Rust releases. Beware of relying on them inside of your code!
///
/// # Examples
/// ```
/// use transient::{TypeId, Any, Transient};
///
/// let static_str = "cookie_monster";
/// // 'static types have a `TypeId` just like in the `std::any` module (as long
/// // as they implement the `Transient` trait, which &'static str does); however,
/// // we use the `Transient::static_type_id` method or `TypeId::of_val` function
/// // instead of `Any::type_id` when dealing with concrete types to avoid needing
/// // to use the fully qualified path (see `Any::type_id` docs).
/// assert_eq!(static_str.static_type_id(), TypeId::of::<&'static str>());
/// assert_eq!(TypeId::of_val(&static_str), TypeId::of::<&'static str>());
/// {
///     let temp_string = static_str.to_string();
///     let temp_str: &str = &temp_string;
///     // unlike `std::any`, non-'static types also have a `TypeId`
///     assert_eq!(temp_str.static_type_id(), TypeId::of::<&'_ str>());
///     // note that this `TypeId` will match regardless of the lifetime
///     assert_eq!(temp_str.static_type_id(), TypeId::of::<&'static str>());
/// }
/// // this `TypeId` can also be compared to a `std::any::TypeId`
/// assert_eq!(TypeId::of::<&'_ str>(), std::any::TypeId::of::<&'static str>());
/// ```
/// [`T::Static`]: Transient::Static
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypeId(std::any::TypeId);

impl TypeId {
    /// Returns the `TypeId` of the [`Transient`] type this generic function
    /// has been instantiated with.
    ///
    /// See the [`TypeId`] documentation for a discussion of the subtle differences
    /// between this identifier and the `std::any::TypeId`.
    ///
    /// # Examples
    ///
    /// ```
    /// use transient::{Transient, Any, TypeId};
    ///
    /// fn is_string_slice<T: Transient>(_s: &T) -> bool {
    ///     TypeId::of::<&str>() == TypeId::of::<T>()
    /// }
    ///
    /// let string = "cookie monster".to_string();
    /// let string_slice: &str = &string;
    ///
    /// assert_eq!(is_string_slice(&0), false);
    /// assert_eq!(is_string_slice(&string), false);
    /// assert_eq!(is_string_slice(&string_slice), true);
    /// assert_eq!(is_string_slice(&"cookie monster"), true);
    /// ```
    #[inline]
    pub fn of<T: Transient>() -> Self {
        let () = T::CHECK;
        TypeId(std::any::TypeId::of::<T::Static>())
    }

    /// Returns the `TypeId` for the type of the given value.
    ///
    /// This is effectively the same as [`TypeId::of`], but allows a value to
    /// provided so that type inference can be used to get the type `T` instead
    /// of needing to explicitly specify it.
    ///
    /// See the [`TypeId`] documentation for a discussion of the subtle differences
    /// between this identifier and the [`std::any::TypeId`].
    #[inline]
    pub fn of_val<T: Transient>(_value: &T) -> Self {
        TypeId::of::<T>()
    }
}

impl From<std::any::TypeId> for TypeId {
    #[inline]
    fn from(value: std::any::TypeId) -> Self {
        TypeId(value)
    }
}

impl From<TypeId> for std::any::TypeId {
    #[inline]
    fn from(value: TypeId) -> Self {
        value.0
    }
}

impl PartialEq<std::any::TypeId> for TypeId {
    #[inline]
    fn eq(&self, other: &std::any::TypeId) -> bool {
        self.0.eq(other)
    }
}

impl std::fmt::Debug for TypeId {
    #[inline]
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::hash::Hash for TypeId {
    #[inline]
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[cfg(test)]
#[allow(unused)]
mod tests {
    use super::*;

    #[test]
    fn test_primative() {
        use crate::{Co, Inv};

        let val = 5;
        let valref = &val;
        let valrefref = &valref;

        let erased: Vec<&dyn Any<Co>> = vec![&5, &valref, &valrefref];
        assert_eq!(erased[0].downcast_ref::<i32>().unwrap(), &5);
        assert_eq!(erased[1].downcast_ref::<&i32>().unwrap(), &valref);
        assert_eq!(erased[2].downcast_ref::<&&i32>().unwrap(), &valrefref);

        // owned `usize`
        let value = 5_usize;
        let _: Box<dyn Any<()>> = Box::new(5_usize);

        // borrowed `usize`
        let _: &dyn Any = &value;
        let _co: &dyn Any<Co> = &value;
        let x = _co.transcend_ref::<Inv>();
        let _: &usize = _co.downcast_ref().unwrap();

        // owned `&usize`
        let r: &usize = &value;
        let x: Box<dyn Any<Co>> = Box::new(r);
        let _: Box<dyn Any<Inv>> = Box::new(&value);
        let y: Box<&usize> = x.downcast::<&usize>().unwrap();

        // borrowed `&usize`
        let valref = &5_usize;
        let _: &dyn Any<Inv> = &valref;
        let co: &dyn Any<Co> = &valref;
        let _: &&usize = co.downcast_ref().unwrap();
        let inv = co.transcend_ref::<Inv>();

        // owned `&&usize`
        let _: Box<dyn Any<(Inv, Inv)>> = Box::new(&valref);
        let _: Box<dyn Any<(Co, Inv)>> = Box::new(&valref);
        let _: Box<dyn Any<(Co, Co)>> = Box::new(&valref);

        // borrowed `&&usize`
        let valrefref = &valref;
        let _: &dyn Any<(Inv, Inv)> = &valrefref;
        let _: &dyn Any<(Co, Inv)> = &valrefref;
        let _: &dyn Any<(Inv, Co)> = &valrefref;
        let _: &dyn Any<(Co, Co)> = &valrefref;

        // borrowed `&mut &mut usize`
        let mut mutval = 5_usize;
        let mut mutref = &mut mutval;
        let mut mutrefref = &mut mutref;

        let _: &dyn Any<(Inv, Inv)> = &mutrefref;
        let _: &dyn Any<(Co, Inv)> = &mutrefref;
        let _co: &dyn Any<Co> = &valref;
        let _: &dyn Any<Inv> = _co.transcend_ref();
        let _: &mut dyn Any<(Co, Inv)> = &mut mutrefref;

        let erased: Box<dyn Any> = Box::new(5_usize);
        let _: Box<usize> = erased.downcast().unwrap();

        let erased: &dyn Any = &value;
        let _: &usize = erased.downcast_ref().unwrap();
    }

    #[test]
    fn test_custom() {
        use crate::{Co, Contra, Inv};

        fn contra_to_inv<'a, 'b: 'a>(arg1: &'a dyn Any<Contra<'a>>) -> &'a dyn Any<Inv<'b>> {
            arg1.transcend_ref()
        }

        fn co_to_inv<'a, 'b: 'a>(arg1: &'a dyn Any<Co<'b>>) -> &'a dyn Any<Inv<'a>> {
            arg1.transcend_ref()
        }

        #[derive(Debug, Clone)]
        pub struct Usize(usize);

        unsafe impl Transient for Usize {
            type Static = Usize;
            type Transience = ();
        }

        #[derive(Debug, Clone, Copy)]
        pub struct UsizeRef<'a>(&'a usize);

        unsafe impl<'a> Transient for UsizeRef<'a> {
            type Static = UsizeRef<'static>;
            type Transience = Co<'a>;
        }

        // owned `Usize`
        let usize_ = Usize(5_usize);
        let stc: Box<dyn Any<()>> = Box::new(usize_.clone());
        let inv: Box<dyn Any<Inv>> = Box::new(usize_.clone());
        let co: Box<dyn Any<Co>> = Box::new(usize_.clone());
        let tr: Box<dyn Any<Inv>> = co.transcend();
        assert_eq!(tr.downcast::<Usize>().unwrap().0, 5_usize);

        // borrowed `Usize`
        let stc: &dyn Any<()> = &usize_;
        let inv: &dyn Any<Inv> = &usize_;
        let co: &dyn Any<Co> = &usize_;
        let tr: &dyn Any<Inv> = co.transcend_ref();
        assert_eq!(tr.downcast_ref::<Usize>().unwrap().0, 5_usize);

        // owned `UsizeRef`
        let usize_ref = UsizeRef(&usize_.0);
        let inv: Box<dyn Any<Inv>> = Box::new(usize_ref.clone());
        let co: Box<dyn Any<Co>> = Box::new(usize_ref.clone());
        let tr: Box<dyn Any<Inv>> = co.transcend();
        assert_eq!(tr.downcast::<UsizeRef>().unwrap().0, &5_usize);

        // borrowed `UsizeRef`
        let inv: &dyn Any<Inv> = &usize_ref;
        let co: &dyn Any<Co> = &usize_ref;
        let tr: &dyn Any<Inv> = co.transcend_ref();
        assert_eq!(tr.downcast_ref::<UsizeRef>().unwrap().0, &5_usize);
    }
}
