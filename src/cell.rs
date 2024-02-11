/*!
Defines safe wrappers for erasing and restoring transient types.

This module defines the `Erased`, `ErasedRef`, and `ErasedMut` structs for
wrapping owned values, shared references, and mutable references, respectively,
that have been transmuted to `'static` and cast to `dyn Any`. While artificially
extending lifetimes is typically very unsafe, each wrapper struct provides a
safe interface to the falsely-`'static` value it wraps by restricting safe
access to it until the *true* lifetime has been restored.

In order to enforce this restriction, the safe public API does not expose the
wrapped value directly, which in principle could be downcast and cloned to
obtain a transient value with an unbounded lifetime. However, this restriction
is lifted when the *true* lifetime is static, since use-after-free is no longer
a concern.
*/
#![allow(missing_docs, unused, dead_code)]

use std::{
    any::{Any, TypeId},
    marker::PhantomData,
    mem,
};
use crate::{
    transient::{Transient},
    storage::{
        Storage, Owned, Ref, Mut,
        box_static_from, val_static_from, val_static_into
    },
    transience::{
        Transience, IntoTransience, Static,
        Invariant, Covariant, Contravariant,
    }
};
use crate::transient::{Static as _};


/// Safely wraps a potentially non-`'static` value that has been transmuted
/// to `'static` and cast to `Box<dyn Any>`.
#[derive(Clone, Debug)]
#[repr(transparent)]
pub struct TransientCell<Data: ?Sized, R: Transience> (
    PhantomData<R>,
    Data,
);

/// Constructors
impl<Data: ?Sized, R: Transience> TransientCell<Data, R> {

    pub fn new<T>(value: T) -> TransientCell<Data, R>
    where
        Data: Sized,
        T: Transient<Static=Data>,
        T::Transience: IntoTransience<R>,
    {
        unsafe { construct_cell(val_static_from(value)) }
    }

    pub const fn new_box<T>(boxed: Box<T>) -> Box<TransientCell<T::Static, R>>
    where
        T: Transient,
        T::Transience: IntoTransience<R>,
    {
        unsafe { mem::transmute(boxed) }
    }

    pub const fn new_ref<T>(value: &T) -> &TransientCell<T::Static, R>
    where
        T: Transient,
        T::Transience: IntoTransience<R>,
    {
        unsafe { mem::transmute(value) }
    }

    pub fn new_mut<T>(value: &mut T) -> &mut TransientCell<T::Static, R>
    where
        T: Transient,
        T::Transience: IntoTransience<R>,
    {
        unsafe { mem::transmute(value) }
    }
}

/// Erasure methods
impl<Data, R: Transience> TransientCell<Data, R> {

    /// Convert the `Transience` of this cell to a valid subtype.
    pub const fn erase(self: Box<Self>) -> Box<TransientCell<dyn Any, R>>

    where
        Data: Transient
    {
        let boxed: Box<Data> = self.as_box().1;
        let static_: Box<Data::Static> = unsafe {  mem::transmute(boxed) };
        unsafe { mem::transmute(static_ as Box<dyn Any>) }
    }
    /// Convert the `Transience` of this cell to a valid subtype.
    pub const fn as_erased(&self) -> &TransientCell<dyn Any, R>
    where
        Data: Transient,
    {
        let static_: &Data::Static = unsafe { mem::transmute(&self.1) };
        unsafe { mem::transmute(static_ as &dyn Any) }
    }
    /// Convert the `Transience` of this cell to a valid subtype.
    pub const fn as_erased_mut(&mut self) -> &mut TransientCell<dyn Any, R>
    where
        Data: Transient,
    {
        let static_: &mut Data::Static = unsafe { mem::transmute(&mut self.1) };
        unsafe { mem::transmute(static_ as &mut dyn Any) }
    }
}
impl<R: Transience> TransientCell<dyn Any, R> {

    pub fn downcast<T>(self: Box<Self>) -> Result<Box<TransientCell<T::Static, R>>, Box<Self>>
    where
        T: Transient<Transience=R>
    {
        if ! (*self).1.is::<T::Static>() {
            Err(self)
        } else {
            let static_: Box<T::Static> = self.as_box().1.downcast()
                .expect("we just checked the type id");
            Ok(unsafe { mem::transmute::<_, Box<TransientCell<_, R>>>(static_) })
        }
    }
    pub fn downcast_ref<T>(&self) -> Option<&TransientCell<T::Static, R>>
    where
        T: Transient<Transience=R>
    {
        let static_: &T::Static = self.as_ref().1.downcast_ref()?;
        Some(unsafe { mem::transmute::<_, &TransientCell<_, R>>(static_) })
    }
    pub fn downcast_mut<T>(&mut self) -> Option<&mut TransientCell<T::Static, R>>
    where
        T: Transient<Transience=R>
    {
        let static_: &mut T::Static = self.as_mut().1.downcast_mut()?;
        Some(unsafe { mem::transmute::<_, &mut TransientCell<_, R>>(static_) })
    }
}

/// Public *safe* API.
impl<Data: ?Sized, R: Transience> TransientCell<Data, R> {

    /// Convert the `Transience` of this cell to a valid subtype.
    pub const fn transcend<R2: Transience>(self) -> TransientCell<Data, R2>
    where
        Data: Sized,
        R: IntoTransience<R2>
    {
        // The trait bound guarantees compatibility
        unsafe{ construct_cell(self.1) }
    }

    /// Convert the `Transience` of this cell to a valid subtype.
    pub const fn transcend_box<R2: Transience>(self: Box<Self>) -> TransientCell<Box<Data>, R2>
    where
        R: IntoTransience<R2>
    {
        // The trait bound guarantees compatibility
        unsafe{ mem::transmute(self) }
    }

    /// Convert the `Transience` of this cell to a valid subtype.
    pub const fn transcend_ref<R2>(&self) -> &TransientCell<Data, R2>
    where
        R2: Transience,
        R: IntoTransience<R2>
    {
        // The trait bound guarantees compatibility
        unsafe{ mem::transmute(self) }
    }

    /// Convert the `Transience` of this cell to a valid subtype.
    pub fn transcend_mut<R2>(&mut self) -> &mut TransientCell<Data, R2>
    where
        R2: Transience,
        R: IntoTransience<R2>
    {
        // The trait bound guarantees compatibility
        unsafe{ mem::transmute(self) }
    }

    /// Safely restore the lifetime of the wrapped value.
    pub fn restore<T>(self) -> T
    where
        Data: Sized,
        T: Transient<Static=Data, Transience=R>
    {
        // The bound on `T` guarantees compatibility
        unsafe { val_static_into(self.1) }
    }
}

pub type ErasedCell<R: Transience> = TransientCell<dyn Any, R>;

pub type ErasedBox<R: Transience> = Box<ErasedCell<R>>;
pub type ErasedRef<'b, R: Transience> = &'b ErasedCell<R>;
pub type ErasedMut<'b, R: Transience> = &'b mut ErasedCell<R>;

fn f() {

    let value = 5;
    let boxed: Box<dyn Any> = Box::new(5);
    let anyref: &dyn Any = &5;

    // let eref: ErasedRef<()> = TransientCell::new_ref(anyref);
    // let ebox: ErasedBox<()> = TransientCell::new_box(boxed);

    // let _: ErasedRef<()> = ebox.as_ref();


    ;()
}


/// Public `unsafe` API.
///
/// This `impl` block defines public methods that require extra considerations
/// to use safely. However, while these functions are marked as `unsafe`, the
/// invariants of the `TransientCell` limit scope of the un-safety and make
/// it relatively easy to use them safely.
///
/// # Inherited Invariants
/// The following properties can be assumed to hold, and do not require further
/// safety considerations when calling (or implementing) the methods:
/// - `D` has the same layout as `TransientCell` and can be transmuted freely.
/// - The `inner` value is still valid as long as the wrapper, or a reference
/// to it, is in scope.
/// - The inner value of any `TransientCell<D, R>` has a true transience that
/// is a subtype of `R`.
impl<D, R: Transience> TransientCell<D, R> {

    /// # SAFETY
    /// The true transience of `D` should be a supertype of `R`; if it isn't,
    /// calling this method may still be fine but the returned wrapper must
    /// not be used in any variance shenanigans.
    pub unsafe fn transcend_unchecked<R2>(self) -> TransientCell<D, R2>
    where
        R2: Transience
    {
        construct_cell(self.1)
    }

    /// Erase and wrap a transient value with lifetime `'src` and variance `V`.
    ///
    /// This constructor *does not* respect the variance defined by `T::Transience`,
    /// and instead matches the transience `R` for the impl (typically inferred at
    /// the call site). This makes the method `unsafe`, as choosing the wrong
    /// transience can lead to UB in some cases.
    ///
    /// To get a wrapper with variance determined by `T::Transience` instead, call
    /// the `Erased::new` method.
    ///
    /// # SAFETY
    /// The returned wrapper must not be used in any variance shenanigans that are
    /// not appropriate for the true `Transience` of `D`.
    pub const unsafe fn transcend_ref_unchecked<R2>(&self) -> &TransientCell<D, R2>
    where
        R2: Transience
    {
        mem::transmute(self)
    }

    pub unsafe fn map<D2, R2>(self, f: impl FnOnce(D) -> D2) -> TransientCell<D2, R2>
    where
        R2: Transience,
    {
        construct_cell(f(self.1))
    }

    pub unsafe fn into_inner(self) -> D {
        self.1
    }

    pub const unsafe fn inner(&self) -> &D {
        &self.1
    }

    pub unsafe fn inner_mut(&mut self) -> &mut D {
        &mut self.1
    }
}

impl<D: ?Sized, R: Transience> TransientCell<D, R>
{
    pub const fn as_ref(&self) -> TransientCell<&D, R> {
        unsafe { mem::transmute(&self.1) }
    }
    pub fn as_mut(&mut self) -> TransientCell<&mut D, R> {
        unsafe { mem::transmute(&mut self.1) }
    }
    pub const fn as_box(self: Box<Self>) -> TransientCell<Box<D>, R> {
        unsafe { mem::transmute(self) }
    }
}

/// # Construct a `TimeSplit` with from the given data with the
///
/// # Safety
/// At least one of these two conditions must be upheld:
/// - The data must be compatible with the requested `Transience`.
/// - The returned `TransientCell` must be used carefully to avoid any kind of
/// illegal variance shenanigans, and must not be released to safe code until
/// the proper transience has been restored.
#[inline(always)]
pub(crate) const unsafe fn construct_cell<D, R>(data: D) -> TransientCell<D, R>
where
    R: Transience
{
    TransientCell(PhantomData, data)
}
