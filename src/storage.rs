use std::{any::Any, mem};
use super::transient::{Transient};


/// Unsafe trait for types that store erased values without protection.
///
/// # Safety
/// `Storage` implementations are not intended to provide a safe API, and
/// must be placed in a wrapper to protect them from misuse. As such, they
/// should never be released to safe code.
pub unsafe trait Storage<'b>: Sized + 'b {
    type Input<T: Transient + 'b>: Sized;

    /// # Safety
    /// The returned storage is *unprotected* and must be used carefully so
    /// that it is not released to safe code. In particular, it must not be
    /// used after `T::Transience` allows, which the borrow checker will not
    /// enforce due to the falsely `'static` lifetime.
    unsafe fn erase<T: Transient>(value: Self::Input<T>) -> Self;

    /// # Safety
    /// `T::Transience` must be compatible with the erased value's `Transience`.
    unsafe fn restore<T: Transient>(self) -> Result<Self::Input<T>, Self>;

    /// # Safety
    /// The returned reference is *unprotected* and must be used carefully
    /// so that it is not released to safe code.
    unsafe fn as_dyn(&self) -> &dyn Any;
}

#[repr(transparent)]
pub struct Boxed(Box<dyn Any>);




pub type Owned = Box<dyn Any>;
pub type Ref<'b> = &'b dyn Any;
pub type Mut<'b> = &'b mut dyn Any;

pub type BoxVec = Vec<Owned>;


unsafe impl<'b> Storage<'b> for Owned {
    type Input<T: Transient + 'b> = T;

    /// # Safety
    /// The returned storage is *unprotected* and must be used carefully so
    /// that it is not released to safe code. In particular, it must not be
    /// used after `T::Transience` allows, which the borrow checker will not
    /// enforce due to the falsely `'static` lifetime.
    unsafe fn erase<T: Transient>(value: T) -> Self {
        let boxed = Box::new(value);
        let extended: Box<T::Static> = box_static_from::<T>(boxed);
        extended as Box<dyn Any>
    }
    /// # Safety
    /// `T::Transience` must be compatible with the erased value's `Transience`.
    unsafe fn restore<T: Transient>(self) -> Result<T, Self> {
        let restored: Box<T::Static> = self.downcast()?;
        let shortened: Box<T> = box_static_into::<T>(restored);
        Ok(*shortened)
    }
    /// The returned reference is *unprotected* and must be used carefully
    /// so that it is not released to safe code.
    unsafe fn as_dyn(&self) -> &dyn Any {
        &**self
    }
}

unsafe impl<'b> Storage<'b> for BoxVec {
    type Input<T: Transient + 'b> = Vec<T>;

    /// # Safety
    /// The returned storage is *unprotected* and must be used carefully so
    /// that it is not released to safe code. In particular, it must not be
    /// used after `T::Transience` allows, which the borrow checker will not
    /// enforce due to the falsely `'static` lifetime.
    unsafe fn erase<T: Transient + 'b>(values: Vec<T>) -> Self {
        values.into_iter()
            .map(|item| box_static_from::<T>(Box::new(item)) as Box<dyn Any>)
            .collect()
    }

    /// # Safety
    /// `T::Transience` must be compatible with the erased value's `Transience`.
    unsafe fn restore<T: Transient + 'b>(self) -> Result<Vec<T>, Self> {
        let n = self.len();
        let mut good_so_far = true;
        let mut successes: Vec<T> = Vec::with_capacity(n);
        let mut recovered: Vec<Box<dyn Any>> = Vec::new();

        for item in self {
            if !good_so_far {
                recovered.push(item)
            } else {
                match item.downcast::<T::Static>() {
                    Ok(restored) => {
                        successes.push(*box_static_into::<T>(restored));
                    }
                    Err(item) => {
                        good_so_far = false;
                        recovered.reserve_exact(n);
                        recovered.extend(
                            successes.drain(..).map(
                                |item| box_static_from(
                                Box::new(item)) as Box<dyn Any>));
                        recovered.push(item);
                    }
                }
            }
        }
        if good_so_far {
            Ok(successes)
        } else {
            Err(recovered)
        }
    }
    /// The returned reference is *unprotected* and must be used carefully
    /// so that it is not released to safe code.
    unsafe fn as_dyn(&self) -> &dyn Any {
        self.first().expect("the vec must contain at least one item")
    }
}

unsafe impl<'b> Storage<'b> for Ref<'b> {
    type Input<T: Transient + 'b> = &'b T;

    /// # Safety
    /// The returned storage is *unprotected* and must be used carefully so
    /// that it is not released to safe code. In particular, it must not be
    /// used after `T::Transience` allows, which the borrow checker will not
    /// enforce due to the falsely `'static` lifetime.
    unsafe fn erase<T: Transient>(value: &'b T) -> Self {
        let extended: &'b T::Static = ref_static_from::<T>(value);
        extended as &'b dyn Any
    }

    /// # Safety
    /// `T::Transience` must be compatible with the erased value's `Transience`.
    unsafe fn restore<T: Transient>(self) -> Result<&'b T, Self> {
        match self.downcast_ref::<T::Static>() {
            Some(restored) => {
                let shortened: &T = ref_static_into::<T>(restored);
                Ok(shortened)
            },
            None => Err(self)
        }
    }

    /// The returned reference is *unprotected* and must be used carefully
    /// so that it is not released to safe code.
    unsafe fn as_dyn(&self) -> &dyn Any {
        *self
    }
}

unsafe impl<'b> Storage<'b> for Mut<'b> {
    type Input<T: Transient + 'b> = &'b mut T;

    /// # Safety
    /// The returned storage is *unprotected* and must be used carefully so
    /// that it is not released to safe code. In particular, it must not be
    /// used after `T::Transience` allows, which the borrow checker will not
    /// enforce due to the falsely `'static` lifetime.
    unsafe fn erase<T: Transient>(value: &'b mut T) -> Self {
        let extended: &'b mut T::Static = unsafe {mut_static_from::<T>(value)};
        extended as &'b mut dyn Any
    }

    /// # Safety
    /// `T::Transience` must be compatible with the erased value's `Transience`.
    unsafe fn restore<T: Transient>(self) -> Result<&'b mut T, Self> {
        match self.downcast_mut::<T::Static>() {
            Some(restored) => {
                let shortened = unsafe {mut_static_into::<T>(restored)};
                Ok(shortened)
            },
            None => Err(self)
        }
    }

    /// The returned reference is *unprotected* and must be used carefully
    /// so that it is not released to safe code.
    unsafe fn as_dyn(&self) -> &dyn Any {
        &**self
    }
}


/// # Safety
/// The falsely-`'static` return value must be properly protected from
/// use outside of its `Transience` bounds.
pub(crate) unsafe fn val_static_from<T: Transient>(value: T) -> T::Static {
    // We know `T` and `T::Static` are compatible
    let lengthened = unsafe {
        mem::transmute_copy::<T, T::Static>(&value)
    };
    mem::forget(value);
    lengthened
}

/// # Safety
/// The given value must be valid for `T::Transience`.
pub(crate) unsafe fn val_static_into<T: Transient>(value: T::Static) -> T {
    let shortened = unsafe {
        // We know `T` and `T::Static` are compatible
        mem::transmute_copy::<T::Static, T>(&value)
    };
    mem::forget(value);
    shortened
}

/// # Safety
/// The falsely-`'static` return value must be properly protected from
/// use outside of its `Transience` bounds.
#[inline(always)]
pub(crate) const unsafe fn box_static_from<T: Transient>(value: Box<T>) -> Box<T::Static> {
    mem::transmute(value)
}
/// # Safety
/// The given value must be valid for `T::Transience`.
#[inline(always)]
pub(crate) const unsafe fn box_static_into<T: Transient>(static_: Box<T::Static>) -> Box<T> {
    mem::transmute(static_)
}
/// # Safety
/// The falsely-`'static` return value must be properly protected from
/// use outside of its `Transience` bounds.
#[inline(always)]
pub(crate) const unsafe fn ref_static_from<T: Transient>(value: &T) -> &T::Static {
    mem::transmute(value)
}
/// # Safety
/// The given value must be valid for `T::Transience`.
#[inline(always)]
pub(crate) const unsafe fn ref_static_into<T: Transient>(static_: &T::Static) -> &T {
    mem::transmute(static_)
}
/// # Safety
/// The falsely-`'static` return value must be properly protected from
/// use outside of its `Transience` bounds.
#[inline(always)]
pub(crate) unsafe fn mut_static_from<T: Transient>(value: &mut T) -> &mut T::Static {
    mem::transmute(value)
}
/// # Safety
/// The given value must be valid for `T::Transience`.
#[inline(always)]
pub(crate) unsafe fn mut_static_into<'b, T: Transient>(static_: &mut T::Static) -> &'b mut T {
    mem::transmute(static_)
}
