#![allow(unused, dead_code)]

use std::{
    iter::{Iterator, IntoIterator, FromIterator},
    marker::PhantomData,
};

use crate::*;


/// [`Iterator`] adapter that [`erases`][TransientAny::erase] the type
/// of each *owned value* produced by the iterator it adapts.
pub struct ErasedVals<'src, I, T>
where
    I: Iterator<Item=T>,
    T: TransientAny<'src>,
{
    inner: I,
    lifetime: PhantomData<&'src ()>,
}

/// [`Iterator`] adapter that attempts to [`restore`][Erased::restore] the
/// type of each (erased) *owned value* produced by the iterator it adapts.
pub struct RestoredVals<'src, I, T>
where
    I: Iterator<Item=Erased<'src>>,
    T: TransientAny<'src>,
{
    inner: I,
    lifetime: PhantomData<&'src T>,
}


/// [`Iterator`] adapter that [`erases`][TransientAny::erase_ref] the type
/// of each *shared reference* produced by the iterator it adapts.
pub struct ErasedRefs<'b, 'src: 'b, I, T>
where
    I: Iterator<Item=&'b T>,
    T: TransientAny<'src>,
{
    inner: I,
    lifetime: PhantomData<&'src ()>,
}

/// [`Iterator`] adapter that attempts to [`restore`][ErasedRef::restore] the
/// type of each (erased) *shared reference* produced by the iterator it adapts.
pub struct RestoredRefs<'b, 'src: 'b, I, T>
where
    I: Iterator<Item=ErasedRef<'b, 'src>>,
    T: TransientAny<'src>,
{
    inner: I,
    lifetime: PhantomData<&'src T>,
}

/// [`Iterator`] adapter that [`erases`][TransientAny::erase_mut] the type
/// of each *mutable reference* produced by the iterator it adapts.
pub struct ErasedMuts<'b, 'src: 'b, I, T>
where
    I: Iterator<Item=&'b mut T>,
    T: TransientAny<'src>,
{
    inner: I,
    lifetime: PhantomData<&'src ()>,
}

/// [`Iterator`] adapter that attempts to [`restore`][ErasedMut::restore] the
/// type of each (erased) *mutable reference* produced by the iterator it adapts.
pub struct RestoredMuts<'b, 'src: 'b, I, T>
where
    I: Iterator<Item=ErasedMut<'b, 'src>>,
    T: TransientAny<'src>,
{
    inner: I,
    lifetime: PhantomData<&'src T>,
}


impl<'src, T, I> Iterator for ErasedVals<'src, I, T>
where
    I: Iterator<Item=T>,
    T: TransientAny<'src>,
{
    type Item = Erased<'src>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.erase())
    }
}

impl<'src, T, I> Iterator for RestoredVals<'src, I, T>
where
    I: Iterator<Item=Erased<'src>>,
    T: TransientAny<'src>,
{
    type Item = Result<T, I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.restore())
    }
}

impl<'b, 'src: 'b, T, I> Iterator for ErasedRefs<'b, 'src, I, T>
where
    I: Iterator<Item=&'b T>,
    T: TransientAny<'src>,
{
    type Item = ErasedRef<'b, 'src>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.erase_ref())
    }
}

impl<'b, 'src: 'b, T, I> Iterator for RestoredRefs<'b, 'src, I, T>
where
    I: Iterator<Item=ErasedRef<'b, 'src>>,
    T: TransientAny<'src>,
{
    type Item = Result<&'b T, I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.restore())
    }
}

impl<'b, 'src: 'b, T, I> Iterator for ErasedMuts<'b, 'src, I, T>
where
    I: Iterator<Item=&'b mut T>,
    T: TransientAny<'src>,
{
    type Item = ErasedMut<'b, 'src>;

    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.erase_mut())
    }
}

impl<'b, 'src: 'b, T, I> Iterator for RestoredMuts<'b, 'src, I, T>
where
    I: Iterator<Item=ErasedMut<'b, 'src>>,
    T: TransientAny<'src>,
{
    type Item = Result<&'b mut T, I::Item>;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.inner.next()?;
        Some(next.restore())
    }
}

/// An [`Iterator`] blanket implementation providing adaptors that
/// [`erase`][TransientAny::erase] or [`restore`][Erased::restore]
/// the type of each item produced by the adapted iterator.
pub trait IterErase: Iterator {

    /// Returns an [`Iterator`] adapter that [`erases`][TransientAny::erase]
    /// the type of each *owned value* produced by the iterator it wraps.
    fn erased<'src>(self) -> ErasedVals<'src, Self, Self::Item>
    where
        Self: Sized,
        Self::Item: TransientAny<'src>,
    {
        ErasedVals {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`erases`][TransientAny::erase_ref]
    /// the type of each *shared reference* produced by the iterator it wraps.
    fn erased_refs<'b, 'src: 'b, T>(self) -> ErasedRefs<'b, 'src, Self, T>
    where
        Self: Sized + Iterator<Item = &'b T>,
        T: TransientAny<'src>,
    {
        ErasedRefs {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`erases`][TransientAny::erase_mut]
    /// the type of each *mutable reference* produced by the iterator it wraps.
    fn erased_muts<'b, 'src: 'b, T>(self) -> ErasedMuts<'b, 'src, Self, T>
    where
        Self: Sized + Iterator<Item = &'b mut T>,
        T: TransientAny<'src>,
    {
        ErasedMuts {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`restores`][Erased::restore]
    /// the type of each (erased) *owned value* produced by the iterator it wraps.
    fn restored<'src, T>(self) -> RestoredVals<'src, Self, T>
    where
        Self: Sized,
        Self: Iterator<Item=Erased<'src>>,
        T: TransientAny<'src>,
    {
        RestoredVals {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`restores`][ErasedRef::restore]
    /// the type of each (erased) *shared reference* produced by the iterator it wraps.
    fn restored_refs<'b, 'src: 'b, T>(self) -> RestoredRefs<'b, 'src, Self, T>
    where
        Self: Sized,
        Self: Iterator<Item=ErasedRef<'b, 'src>>,
        T: TransientAny<'src>,
    {
        RestoredRefs {inner: self, lifetime: PhantomData}
    }

    /// Returns an [`Iterator`] adapter that [`restores`][ErasedMut::restore]
    /// the type of each (erased) *mutable reference* produced by the iterator it wraps.
    fn restored_muts<'b, 'src: 'b, T>(self) -> RestoredMuts<'b, 'src, Self, T>
    where
        Self: Sized,
        Self: Iterator<Item=ErasedMut<'b, 'src>>,
        T: TransientAny<'src>,
    {
        RestoredMuts {inner: self, lifetime: PhantomData}
    }
}

impl<T> IterErase for T where T: Iterator + ?Sized {}


#[cfg(test)]
mod iter_tests {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    struct Transient<'a> (
        &'a str
    );
    unsafe impl<'a> TransientAny<'a> for Transient<'a> {
        type Static = Transient<'static>;
        type Variance = Invariant<'a>;
    }

    #[test]
    pub(super) fn test_owned() {
        let items = vec![Transient("a"), Transient("b")];
        let restored = items.into_iter()
            .erased()
            .restored::<Transient>()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(restored, vec![Transient("a"), Transient("b")]);
    }

    #[test]
    pub(super) fn test_refs() {
        let items = vec![Transient("a"), Transient("b")];
        let restored = items.iter()
            .erased_refs()
            .restored_refs::<Transient>()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(restored, vec![&Transient("a"), &Transient("b")]);
    }

    #[test]
    pub(super) fn test_muts() {
        let mut items = vec![Transient("a"), Transient("b")];
        let restored = items.iter_mut()
            .erased_muts()
            .restored_muts::<Transient>()
            .collect::<Result<Vec<_>, _>>()
            .unwrap();
        assert_eq!(restored, vec![&mut Transient("a"), &mut Transient("b")]);
    }
}
