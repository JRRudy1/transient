
use std::{iter::Iterator, marker::PhantomData};
use crate::{Transient, Erased, ErasedRef, ErasedMut};


/// [`Iterator`] adapter that [`erases`][Transient::erase] the type
/// of each *owned value* produced by the iterator it adapts.
pub struct ErasedVals<I, T>
where
    I: Iterator<Item=T>,
    T: Transient,
{
    inner: I,
    lifetime: PhantomData<T>,
}

/// [`Iterator`] adapter that attempts to [`restore`][Erased::restore] the
/// type of each (erased) *owned value* produced by the iterator it adapts.
pub struct RestoredVals<I, T>
where
    I: Iterator<Item=Erased<T::Transience>>,
    T: Transient,
{
    inner: I,
    lifetime: PhantomData<T>,
}


/// [`Iterator`] adapter that [`erases`][Transient::erase_ref] the type
/// of each *shared reference* produced by the iterator it adapts.
pub struct ErasedRefs<'b, I, T: 'b>
where
    I: Iterator<Item=&'b T>,
    T: Transient,
{
    inner: I,
    lifetime: PhantomData<T>,
}

/// [`Iterator`] adapter that attempts to [`restore`][ErasedRef::restore] the
/// type of each (erased) *shared reference* produced by the iterator it adapts.
pub struct RestoredRefs<'b, I, T>
where
    I: Iterator<Item=ErasedRef<'b, T::Transience>>,
    T: Transient,
{
    inner: I,
    lifetime: PhantomData<T>,
}


/// [`Iterator`] adapter that [`erases`][Transient::erase_mut] the type
/// of each *mutable reference* produced by the iterator it adapts.
pub struct ErasedMuts<'b, I, T: 'b>
where
    I: Iterator<Item=&'b mut T>,
    T: Transient,
{
    inner: I,
    lifetime: PhantomData<T>,
}

/// [`Iterator`] adapter that attempts to [`restore`][ErasedMut::restore] the
/// type of each (erased) *mutable reference* produced by the iterator it adapts.
pub struct RestoredMuts<'b, I, T>
where
    I: Iterator<Item=ErasedMut<'b, T::Transience>>,
    T: Transient,
{
    inner: I,
    lifetime: PhantomData<T>,
}


impl<I, T> Iterator for ErasedVals<I, T>
where
    I: Iterator<Item=T>,
    T: Transient,
{
    type Item = Erased<T::Transience>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.erase())
    }
}

impl<T, I> Iterator for RestoredVals<I, T>
where
    I: Iterator<Item=Erased<T::Transience>>,
    T: Transient,
{
    type Item = Result<T, I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.restore())
    }
}

impl<'b, T: 'b, I> Iterator for ErasedRefs<'b, I, T>
where
    I: Iterator<Item=&'b T>,
    T: Transient,
{
    type Item = ErasedRef<'b, T::Transience>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.erase_ref())
    }
}

impl<'b, T: 'b, I> Iterator for RestoredRefs<'b, I, T>
where
    I: Iterator<Item=ErasedRef<'b, T::Transience>>,
    T: Transient,
{
    type Item = Result<&'b T, I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.restore())
    }
}

impl<'b, T: 'b, I> Iterator for ErasedMuts<'b, I, T>
where
    I: Iterator<Item=&'b mut T>,
    T: Transient,
{
    type Item = ErasedMut<'b, T::Transience>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.erase_mut())
    }
}

impl<'b, T: 'b, I> Iterator for RestoredMuts<'b, I, T>
where
    I: Iterator<Item=ErasedMut<'b, T::Transience>>,
    T: Transient,
{
    type Item = Result<&'b mut T, I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.restore())
    }
}

/// An [`Iterator`] blanket implementation providing adaptors that
/// [`erase`][Transient::erase] or [`restore`][Erased::restore]
/// the type of each item produced by the adapted iterator.
pub trait IterErase: Iterator {

    /// Returns an [`Iterator`] adapter that [`erases`][Transient::erase]
    /// the type of each *owned value* produced by the iterator it wraps.
    fn erased(self) -> ErasedVals<Self, Self::Item>
    where
        Self: Sized,
        Self::Item: Transient,
    {
        ErasedVals {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`erases`][Transient::erase_ref]
    /// the type of each *shared reference* produced by the iterator it wraps.
    fn erased_refs<'b, T: 'b>(self) -> ErasedRefs<'b, Self, T>
    where
        Self: Sized + Iterator<Item = &'b T>,
        T: Transient,
    {
        ErasedRefs {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`erases`][Transient::erase_mut]
    /// the type of each *mutable reference* produced by the iterator it wraps.
    fn erased_muts<'b, T>(self) -> ErasedMuts<'b, Self, T>
    where
        Self: Sized + Iterator<Item = &'b mut T>,
        T: Transient,
    {
        ErasedMuts {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`restores`][Erased::restore]
    /// the type of each (erased) *owned value* produced by the iterator it wraps.
    fn restored<T>(self) -> RestoredVals<Self, T>
    where
        Self: Sized,
        Self: Iterator<Item=Erased<T::Transience>>,
        T: Transient,
    {
        RestoredVals {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`restores`][ErasedRef::restore]
    /// the type of each (erased) *shared reference* produced by the iterator it wraps.
    fn restored_refs<'b, T>(self) -> RestoredRefs<'b, Self, T>
    where
        Self: Sized,
        Self: Iterator<Item=ErasedRef<'b, T::Transience>>,
        T: Transient,
    {
        RestoredRefs {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`restores`][ErasedMut::restore]
    /// the type of each (erased) *mutable reference* produced by the iterator it wraps.
    fn restored_muts<'b, T>(self) -> RestoredMuts<'b, Self, T>
    where
        Self: Sized,
        Self: Iterator<Item=ErasedMut<'b, T::Transience>>,
        T: Transient,
    {
        RestoredMuts {inner: self, lifetime: PhantomData}
    }
}

impl<T> IterErase for T where T: Iterator + ?Sized {}


#[cfg(test)]
mod iter_tests {
    use super::*;
    use crate::Invariant;

    #[derive(Debug, PartialEq, Eq)]
    struct T<'a> (
        &'a str
    );
    unsafe impl<'a> Transient for T<'a> {
        type Static = T<'static>;
        type Transience = Invariant<'a>;
    }

    #[test]
    pub(super) fn test_owned() {
        let items = vec![T("a"), T("b")];
        let restored = items.into_iter()
            .erased()
            .restored::<T>()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(restored, vec![T("a"), T("b")]);
    }

    #[test]
    pub(super) fn test_refs() {
        let items = vec![T("a"), T("b")];
        let restored = items.iter()
            .erased_refs()
            .restored_refs::<T>()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(restored, vec![&T("a"), &T("b")]);
    }

    #[test]
    pub(super) fn test_muts() {
        let mut items = vec![T("a"), T("b")];
        let restored = items.iter_mut()
            .erased_muts()
            .restored_muts::<T>()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(restored, vec![&mut T("a"), &mut T("b")]);
    }
}
