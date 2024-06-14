//! Analogue to the `std::any` module, containing a re-implementation of [`Any`] 
//! with support for non-`'static` types alongside re-exports of [`TypeId`], 
//! [`type_name`], and [`type_name_of_val`].
use crate::{
    transient::Transient,
    transience::{Transience, CanRecoverFrom, CanTranscendTo}
};
pub use std::any::{type_name, type_name_of_val};

/// Re-export
pub use std::any::TypeId;

/// A trait to emulate dynamic typing, with support for non-`'static` types.
/// Methods for this trait object are defined on the [`AnyOps`] and [`UnsafeOps`]
/// extension traits.
///
/// # Differences from `std::any::Any`
/// - Types must first implement (or derive) the [`Transient`] trait before the
/// blanket impl for all `T: Transient` will apply to them.
/// - In addition to importing the `Any` trait, the [`AnyOps`] trait must also
/// be brought into scope for the `dyn Any` methods to become available. An
/// additional extension trait [`UnsafeOps`] can also be imported to gain
/// access to `unsafe` operations on the `dyn Any` object, but make sure to
/// review the safety requirements before using them.
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
/// methods such as [`transcend`][AnyOps::transcend] can be used to adjust the
/// transience at a later time. Note that if the transience is upcast to a
/// shorter lifetime (or a longer lifetime in the *contravariant* case), then
/// it can only be safely [`downcast`][AnyOps::downcast] to the shortened lifetime
/// instead of the original (but if you are brave and/or careful, `unsafe`
/// methods such as [`transcend_unbounded`][UnsafeOps::transcend_unbounded]
/// can be used to get around this).
/// - The `*_unchecked` methods do not require nightly builds.
/// - Only `Box`s using the `Global` allocator are supported.
/// - Only `Sized` types are supported.
pub trait Any<R: Transience = ()> {
    /// Gets the `TypeId` of `self`.
    fn type_id(&self) -> TypeId;
}

impl<T: Transient, R: Transience> Any<R> for T
    where T::Transience: CanTranscendTo<R>,
{
    fn type_id(&self) -> TypeId {
        TypeId::of::<T::Static>()
    }
}


/// Extension trait used to define methods on the [`dyn Any<_>`][Any] trait object.
///
/// This trait has a blanket `impl` for all [`Transient`] types with a compatible
/// [`Transience`], and cannot be implemented directly.
pub trait AnyOps<R: Transience>: UnsafeOps<R> {

    /// Returns `true` if the concrete type of the erased object is `T`.
    ///
    /// Slight caveat: this method is _not actually_ comparing the erased type
    /// (call it `E`) to the given type `T`; in reality, it is comparing
    /// `E::Static` to `T::Static` as defined in their [`Transient`] impls.
    /// Assuming `Transient` was implemented correctly for these types, which
    /// the `unsafe` implementor pinky-promised to do, then this comparison is
    /// essentially equivalent. However, this only verifies the "static identity"
    /// of the type, ignoring any lifetime information, so an erased type
    /// `&'short i32` will return `true` when compared to `&'any i32`, even
    /// when the lifetimes do not match. This is not a problem when used from
    /// from safe code since the other mechanisms in this crate enforce the
    /// lifetime compatibilities, but `unsafe` code **must not** assume that
    /// it can freely cast from `E` to `T` with no regard for lifetimes just
    /// because this method returned `true`.
    fn is<T: Transient>(&self) -> bool;

    /// Attempt to downcast the box to a concrete type with its lifetime
    /// parameters restored, returning the original in the `Err` variant
    /// if the type was incorrect.
    fn downcast<T: Transient>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
        where T::Transience: CanRecoverFrom<R>
    {
        if self.is::<T>() {
            // We just confirmed that the type is correct.
            Ok(unsafe{ self.downcast_unchecked() })
        } else {
            Err(self)
        }
    }


    /// Returns a reference to the inner value with its lifetime parameters
    /// restored if it is of type `T`, or `None` if it isn't.
    fn downcast_ref<T: Transient>(&self) -> Option<&T>
        where T::Transience: CanRecoverFrom<R>
    {
        if self.is::<T>() {
            // We just confirmed that the type is correct.
            Some(unsafe{ self.downcast_ref_unchecked() })
        } else {
            None
        }
    }

    /// Returns a mutable reference to the inner value with its lifetime
    /// parameters restored if it is of type `T`, or `None` if it isn't.
    fn downcast_mut<T: Transient>(&mut self) -> Option<&mut T>
        where T::Transience: CanRecoverFrom<R>
    {
        if self.is::<T>() {
            // We just confirmed that the type is correct.
            Some(unsafe{ self.downcast_mut_unchecked() })
        } else {
            None
        }
    }

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend<R2: Transience>(self: Box<Self>) -> Box<dyn Any<R2>>
        where R: CanTranscendTo<R2>
    {
        // The trait bound on `R` guarantees that the transience is compatible.
        unsafe{ self.transcend_unbounded::<R2>() }
    }

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend_ref<R2: Transience>(&self) -> &dyn Any<R2>
        where R: CanTranscendTo<R2>
    {
        // The trait bound on `R` guarantees that the transience is compatible.
        unsafe{ self.transcend_ref_unbounded::<R2>() }
    }

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend_mut<R2: Transience>(&mut self) -> &mut dyn Any<R2>
        where R: CanTranscendTo<R2>
    {
        // The trait bound on `R` guarantees that the transience is compatible.
        unsafe{ self.transcend_mut_unbounded::<R2>() }
    }
}


/// Extension trait used to define `unsafe` methods on the `dyn Any<_>`
/// trait object.
///
/// This trait has a blanket `impl` for all [`Transient`] types with a compatible
/// [`Transience`], and cannot be implemented directly.
pub trait UnsafeOps<R: Transience> {

    /// Downcasts the box to a concrete type without compile-time checks.
    ///
    /// For a safe alternative see [`downcast`].
    ///
    /// # Safety
    /// The contained value must be of type `T::Static`. Calling this method
    /// with the incorrect type is *undefined behavior*.
    ///
    /// [`downcast`]: AnyOps::downcast
    unsafe fn downcast_unchecked<T: Transient>(self: Box<Self>) -> Box<T>
        where T::Transience: CanRecoverFrom<R>;

    /// Downcasts the shared reference to a concrete type without compile-time checks.
    ///
    /// For a safe alternative see [`downcast_ref`].
    ///
    /// # Safety
    /// The contained value must be of type `T::Static`. Calling this method
    /// with the incorrect type is *undefined behavior*.
    ///
    /// [`downcast_ref`]: AnyOps::downcast_ref
    unsafe fn downcast_ref_unchecked<T: Transient>(&self) -> &T
        where T::Transience: CanRecoverFrom<R>;

    /// Downcasts the mutable reference to a concrete type without compile-time checks.
    ///
    /// For a safe alternative see [`downcast_mut`].
    ///
    /// # Safety
    /// The contained value must be of type `T::Static`. Calling this method
    /// with the incorrect type is *undefined behavior*.
    ///
    /// [`downcast_mut`]: AnyOps::downcast_mut
    unsafe fn downcast_mut_unchecked<T: Transient>(&mut self) -> &mut T
        where T::Transience: CanRecoverFrom<R>;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// For a safe alternative see [`transcend`].
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`. Calling this
    /// method with an incompatible transience can lead to *undefined behavior*.
    ///
    /// [`transcend`]: AnyOps::transcend
    unsafe fn transcend_unbounded<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
        where R2: Transience;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// For a safe alternative see [`transcend_ref`].
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`. Calling this
    /// method with an incompatible transience can lead to *undefined behavior*.
    ///
    /// [`transcend_ref`]: AnyOps::transcend_ref
    unsafe fn transcend_ref_unbounded<R2>(&self) -> &dyn Any<R2>
        where R2: Transience;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// For a safe alternative see [`transcend_mut`].
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`. Calling this
    /// method with an incompatible transience can lead to *undefined behavior*.
    ///
    /// [`transcend_mut`]: AnyOps::transcend_mut
    unsafe fn transcend_mut_unbounded<R2>(&mut self) -> &mut dyn Any<R2>
        where R2: Transience;
}

impl<R: Transience> AnyOps<R> for dyn Any<R> + '_ {
    fn is<T: Transient>(&self) -> bool {
        self.type_id() == TypeId::of::<T::Static>()
    }
}

impl<R: Transience> UnsafeOps<R> for dyn Any<R> + '_ {

    unsafe fn downcast_unchecked<T: Transient>(self: Box<Self>) -> Box<T>
        where T::Transience: CanRecoverFrom<R>,
    {
        // The caller is expected to ensure that the inner type is `T::Static`,
        // which the `Transient` trait guarantees has the same layout as `T`,
        // so the pointer cast is safe. The trait bound on `T::Transience`
        // ensures that the lifetime parameters of the returned type satisfy
        // the necessary subtyping relationships.
        Box::from_raw(Box::into_raw(self).cast())
    }

    unsafe fn downcast_ref_unchecked<T: Transient>(&self) -> &T
        where T::Transience: CanRecoverFrom<R>
    {
        // The caller is expected to ensure that the inner type is `T::Static`,
        // which the `Transient` trait guarantees has the same layout as `T`,
        // so the pointer casts are safe. The trait bound on `T::Transience`
        // ensures that the lifetime parameters of the returned type satisfy
        // the necessary subtyping relationships.
        &*(self as *const Self).cast()
    }

    unsafe fn downcast_mut_unchecked<T: Transient>(&mut self) -> &mut T
        where T::Transience: CanRecoverFrom<R>
    {
        // The caller is expected to ensure that the inner type is `T::Static`,
        // which the `Transient` trait guarantees has the same layout as `T`,
        // so the pointer casts are safe. The trait bound on `T::Transience`
        // ensures that the lifetime parameters of the returned type satisfy
        // the necessary subtyping relationships.
        &mut *(self as *mut Self).cast()
    }

    unsafe fn transcend_unbounded<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
        where R2: Transience
    {
        // `Box<dyn Any<_>>` always has the same layout regardless of the
        // transience so transmuting the type is safe, and the caller is
        // expected to ensure that the transience is compatible.
        std::mem::transmute(self)
    }

    unsafe fn transcend_ref_unbounded<R2>(&self) -> &dyn Any<R2>
        where R2: Transience
    {
        // `&dyn Any<_>` always has the same layout regardless of the
        // transience so transmuting the type is safe, and the caller is
        // expected to ensure that the transience is compatible.
        std::mem::transmute(self)
    }

    unsafe fn transcend_mut_unbounded<R2>(&mut self) -> &mut dyn Any<R2>
        where R2: Transience
    {
        // `&mut dyn Any<_>` always has the same layout regardless of the
        // transience so transmuting the type is safe, and the caller is
        // expected to ensure that the transience is compatible.
        std::mem::transmute(self)
    }
}

impl<R: Transience> std::fmt::Debug for dyn Any<R> + '_ {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Any").finish_non_exhaustive()
    }
}


#[test]
#[allow(unused)]
fn test2() {
    use crate::Co;

    let val = 5;
    let valref = &val;
    let valrefref = &valref;

    let erased: Vec<&dyn Any<Co>> = vec![&5, &valref, &valrefref];
    assert_eq!(erased[0].downcast_ref::<i32>().unwrap(), &5);
    assert_eq!(erased[1].downcast_ref::<&i32>().unwrap(), &valref);
    assert_eq!(erased[2].downcast_ref::<&&i32>().unwrap(), &valrefref);
}


#[test]
#[allow(unused)]
fn test_any<'a>() {
    use crate::{Co, Contra, Inv};

    fn f<'a, 'b: 'a>(arg1: &'a dyn Any<Contra<'a>>) -> &'a dyn Any<Inv<'b>> {
        arg1.transcend_ref()
    }
    #[derive(Debug)]
    pub struct Usize(usize);

    #[derive(Debug, Clone, Copy)]
    pub struct UsizeRef<'a>(&'a usize);
    #[derive(Debug, Clone, Copy)]
    pub struct UsizeRefRef<'a, 'b>(&'a &'b usize);

    unsafe impl Transient for Usize {
        type Static = Usize;
        type Transience = ();
    }
    unsafe impl<'a> Transient for UsizeRef<'a> {
        type Static = UsizeRef<'static>;
        type Transience = Co<'a>;
    }
    unsafe impl<'a, 'b> Transient for UsizeRefRef<'a, 'b> {
        type Static = UsizeRefRef<'static, 'static>;
        type Transience = (Co<'a>, Co<'b>);
    }

    // owned `usize`
    let value = 5_usize;
    // let _: Box<dyn TrAny> = Box::new(5_usize);
    let _: Box<dyn Any<()>> = Box::new(5_usize);

    // borrowed `usize`
    let _: &dyn Any = &value;
    let _co: &dyn Any<Co> = &value;
    let x = _co.transcend_ref::<Inv>();
    let _: &usize = _co.downcast_ref().unwrap();

    // owned `&usize`
    let r: &usize = &value;
    let x: Box<dyn Any<Co>> = Box::new(r);
    // let _: Box<dyn Any<Inv>> = Box::new(&value);
    let y: Box<&usize> = x.downcast::<&usize>().unwrap();
    // let y: Box<&usize> = x.downcast::<&'_ usize>().unwrap();

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
