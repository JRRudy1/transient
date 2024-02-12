use std::{fmt::Debug, marker::PhantomData};

/// Unsafe trait for zero-sized types that can be used to establish the
/// [variance] for each lifetime parameter of a struct.
///
/// Note that even though most types are *covariant* in reality, this crate
/// treats *invariance* as the default since any other assumption could cause
/// undefined behavior if chosen incorrectly. To override this default, the
/// [`Transience`] associated type can be set in the type's `Transient`
/// implementation; if using the derive macro, this corresponds to including
/// the `#[r#unsafe(covariant)]` attribute.
///
/// # SAFETY
/// Must be a zero-sized-type with the variance suggested by it's name.
///
/// [`Transience`]: crate::Transient::Transience
/// [variance]: https://doc.rust-lang.org/nomicon/subtyping.html
pub unsafe trait Transience:
    Debug + Default
    + IntoTransience<Self>
    // + IntoTransience<Self::Frozen>
{
    // type Frozen: Transience;
}

pub unsafe trait Variance<'a>: Transience {}
// pub unsafe trait Variance<'a>: Transience<Frozen=Invariant<'a>> {}


/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a *static* type with no generic lifetime parameters.
///
/// Such types only contain owned data and static references, and are thus much
/// safer to work with and allow several restrictions imposed by the crate to
/// be loosened. For example, the erased wrapper structs typically restrict all
/// access to the inner `dyn Any`, but expose safe public methods for getting
/// accessing it when the wrapped type is `'static`.
pub type Timeless = ();
unsafe impl Transience for Timeless {
    // type Frozen = Self;
}

/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *invariant* with respect to its lifetime parameter `'a`.
///
/// An *invariant* type is one for which the compiler cannot safely assume that
/// its lifetime may be shortened **or** lengthened (such as `&'a mut T`). Such
/// a type must therefore match the expected lifetime exactly when passed to a
/// function.
///
/// See the [`Transience`] documentation for more information.
#[derive(Clone, Copy, Debug, Default)]
pub struct Invariant<'a>( PhantomData<fn(&'a ()) -> &'a ()> );

/// Convenience type alias
pub type Inv<'a> = Invariant<'a>;


/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *covariant* with respect to its lifetime parameter `'a`.
///
/// A *covariant* type is one for which the compiler can safely *shorten* its
/// lifetime parameter as needed when passing it to a function; for example,
/// `&'a T` is *covariant* w.r.t. `'a`, so `&'static str` can be used where
/// `&'short str` is expected.
///
/// See the [`Transience`] documentation for more information.
#[derive(Clone, Copy, Debug, Default)]
pub struct Covariant<'a>( PhantomData<&'a ()> );

/// Convenience type alias
pub type Co<'a> = Covariant<'a>;


/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a type that is *contravariant* with respect to its lifetime parameter `'a`.
///
/// A *contravariant* type is one for which the compiler can safely *lengthen*
/// its lifetime parameter as needed when passing it to a function; for example,
/// `fn(&'a str)` is *contravariant* w.r.t. `'a`, so `fn(&'short str)` can be
/// used where `fn(&'static str)` is expected.
///
/// See the [`Transience`] documentation for more information.
#[derive(Clone, Copy, Debug, Default)]
pub struct Contravariant<'a>( PhantomData<fn(&'a ())> );

/// Convenience type alias
pub type Contra<'a> = Contravariant<'a>;


unsafe impl<'a> Variance<'a> for Invariant<'a> {}
unsafe impl<'a> Transience for Invariant<'a> {}

unsafe impl<'a> Variance<'a> for Covariant<'a> {}
unsafe impl<'a> Transience for Covariant<'a> {}

unsafe impl<'a> Variance<'a> for Contravariant<'a> {}
unsafe impl<'a> Transience for Contravariant<'a> {}


pub unsafe trait SubTransience<Source: ?Sized>: Transience {}

unsafe impl<R1, R2> SubTransience<R1> for R2
where
    R1: Transience + IntoTransience<R2>,
    R2: Transience,
{}


/// Marker trait indicating that the implementing [`Transience`] can safely
/// "downgrade" to the `Other` one.
///
/// # SAFETY
/// This trait must only be implemented for *valid* conversions. Implementing
/// this trait for an *invalid* conversion (such as shortening the lifetime
/// of a *contravariant* type) can lead to undefined behavior.
pub unsafe trait IntoTransience<Other: ?Sized> {}

/// `Timeless` can safely converted to any other `Transience`.
unsafe impl<V: Transience + ?Sized> IntoTransience<V> for Timeless {}

/// `Covariant` can shorten its lifetime.
unsafe impl<'short, 'long: 'short> IntoTransience<Co<'short>> for Co<'long> {}

/// `Covariant` can shorten its lifetime and/or become `Invariant`.
unsafe impl<'short, 'long: 'short>
    IntoTransience<Inv<'short>> for Co<'long> {}

/// `Contravariant` can lengthen its lifetime.
unsafe impl<'short, 'long: 'short>
    IntoTransience<Contra<'long>> for Contra<'short> {}

/// `Contravariant` can lengthen its lifetime and/or become `Invariant`.
unsafe impl<'short, 'long: 'short>
    IntoTransience<Inv<'long>> for Contra<'short> {}

/// `Invariant` can stay the safe.
unsafe impl<'a> IntoTransience<Inv<'a>> for Inv<'a> {}


pub unsafe trait RecoverTransience<From> {}

unsafe impl<V: Transience> RecoverTransience<V> for Timeless {}
unsafe impl<'a> RecoverTransience<Inv<'a>> for Inv<'a> {}
unsafe impl<'a> RecoverTransience<Inv<'a>> for Co<'a> {}
unsafe impl<'a> RecoverTransience<Inv<'a>> for Contra<'a> {}
unsafe impl<'a, 'long: 'a> RecoverTransience<Co<'long>> for Co<'a> {}
unsafe impl<'short, 'a: 'short> RecoverTransience<Contra<'short>> for Contra<'a> {}

macro_rules! impl_tuple {

    { ($($src:ident),*) => ($($dst:ident),*) } => {

        unsafe impl<$($src),*> Transience for ($($src),*,)
        where
            $( $src: Transience ),*
        {}

        unsafe impl<$($src),*, $($dst),*>
            IntoTransience<($($dst),*,)> for ($($src),*,)
        where
            $( $src: IntoTransience<$dst> ),*
        {}

        unsafe impl<$($src),*, $($dst),*>
            RecoverTransience<($($dst),*,)> for ($($src),*,)
        where
            $( $src: RecoverTransience<$dst> ),*
        {}
    }
}

impl_tuple!{ (A1) => (A2) }
impl_tuple!{ (A1, B1) => (A2, B2) }
impl_tuple!{ (A1, B1, C1) => (A2, B2, C2) }
impl_tuple!{ (A1, B1, C1, D1) => (A2, B2, C2, D2) }
