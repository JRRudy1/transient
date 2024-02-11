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
    + IntoTransience<Self::Frozen>
{
    type Frozen: Transience;
}

pub unsafe trait Variance<'a>: Transience<Frozen=Invariant<'a>> {}


/// Used to set the [variance](https://doc.rust-lang.org/nomicon/subtyping.html)
/// of a *static* type with no generic lifetime parameters.
///
/// Such types only contain owned data and static references, and are thus much
/// safer to work with and allow several restrictions imposed by the crate to
/// be loosened. For example, the erased wrapper structs typically restrict all
/// access to the inner `dyn Any`, but expose safe public methods for getting
/// accessing it when the wrapped type is `'static`.
pub type Static = ();
unsafe impl Transience for Static {
    type Frozen = Self;
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


unsafe impl<T1> Transience for (T1,)
where
    T1: Transience + IntoTransience<T1::Frozen>
{
    type Frozen = (T1::Frozen,);
}

unsafe impl<T1: Transience, T2: Transience> Transience for (T1, T2)
where
    T1: IntoTransience<T1::Frozen>,
    T2: IntoTransience<T2::Frozen>,
{
    type Frozen = (T1::Frozen, T2::Frozen);
}

unsafe impl<T1, T2, T3> Transience for (T1, T2, T3)
where
    T1: Transience + IntoTransience<T1::Frozen>,
    T2: Transience + IntoTransience<T2::Frozen>,
    T3: Transience + IntoTransience<T3::Frozen>,
{
    type Frozen = (T1::Frozen, T2::Frozen, T3::Frozen);
}

unsafe impl<T1, T2, T3, T4> Transience for (T1, T2, T3, T4)
where
    T1: Transience + IntoTransience<T1::Frozen>,
    T2: Transience + IntoTransience<T2::Frozen>,
    T3: Transience + IntoTransience<T3::Frozen>,
    T4: Transience + IntoTransience<T4::Frozen>,
{
    type Frozen = (T1::Frozen, T2::Frozen, T3::Frozen, T4::Frozen);
}



unsafe impl<'a> Variance<'a> for Invariant<'a> {}

unsafe impl<'a> Transience for Invariant<'a> {
    type Frozen = Invariant<'a>;
}

unsafe impl<'a> Variance<'a> for Covariant<'a> {}

unsafe impl<'a> Transience for Covariant<'a> {
    type Frozen = Invariant<'a>;
}

unsafe impl<'a> Variance<'a> for Contravariant<'a> {}

unsafe impl<'a> Transience for Contravariant<'a> {
    type Frozen = Invariant<'a>;
}


/*macro_rules! impl_variances {
    { $( <$lt:lifetime> => $ty:ty );* $(;)? } =>
    {$(
        unsafe impl<$lt> Transience for $ty {
            type Frozen = Invariant<$lt>;
        }
        unsafe impl<$lt> Variance<$lt> for $ty {}
    )*}
}
impl_variances!{
    <'a> => Invariant<'a>;
    <'a> => Covariant<'a>;
    <'a> => Contravariant<'a>;
}*/
/*
macro_rules! impl_transience_tuples {
    { $( <$($lt:lifetime),*> => ($($ty:ty ),* $(,)?) );* $(;)? } =>
    {$(
        unsafe impl<$($lt),*> Transience for ($($ty),* ,) {
            type Frozen = ( $( Invariant<$lt> ),* ,);
        }
    )*}
}

impl_transience_tuples!{
    // Invariant
    <'a> => (Invariant<'a>,);
    <'a, 'b> => (Invariant<'a>, Invariant<'b>);
    <'a, 'b, 'c> => (Invariant<'a>, Invariant<'b>, Invariant<'c>);
    <'a, 'b, 'c, 'd> => (Invariant<'a>, Invariant<'b>, Invariant<'c>, Invariant<'d>);
    // Covariant
    <'a> => (Covariant<'a>,);
    <'a, 'b> => (Covariant<'a>, Covariant<'b>);
    <'a, 'b, 'c> => (Covariant<'a>, Covariant<'b>, Covariant<'c>);
    <'a, 'b, 'c, 'd> => (Covariant<'a>, Covariant<'b>, Covariant<'c>, Covariant<'d>);
    // // Contravariant
    <'a> => (Contravariant<'a>,);
    <'a, 'b> => (Contravariant<'a>, Contravariant<'b>);
    <'a, 'b, 'c> => (Contravariant<'a>, Contravariant<'b>, Contravariant<'c>);
    <'a, 'b, 'c, 'd> => (Contravariant<'a>, Contravariant<'b>, Contravariant<'c>, Contravariant<'d>);

    // Mixed: 1*Inv + 1*Co
    <'a, 'b> => (Invariant<'a>, Covariant<'b>);
    <'a, 'b> => (Covariant<'a>, Invariant<'b>);
    // Mixed: 1*Inv + 2*Co
    <'a, 'b, 'c> => (Invariant<'a>, Covariant<'b>, Covariant<'c>);
    <'a, 'b, 'c> => (Covariant<'a>, Invariant<'b>, Covariant<'c>);
    <'a, 'b, 'c> => (Covariant<'a>, Covariant<'b>, Invariant<'c>);
    // Mixed: 2*Inv + 1*Co
    <'a, 'b, 'c> => (Covariant<'a>, Invariant<'b>, Invariant<'c>);
    <'a, 'b, 'c> => (Invariant<'a>, Covariant<'b>, Invariant<'c>);
    <'a, 'b, 'c> => (Invariant<'a>, Invariant<'b>, Covariant<'c>);

    // Mixed: 1*Inv + 1*Contra
    <'a, 'b> => (Invariant<'a>, Contravariant<'b>);
    <'a, 'b> => (Contravariant<'a>, Invariant<'b>);
    // Mixed: 1*Inv + 2*Contra
    <'a, 'b, 'c> => (Invariant<'a>, Contravariant<'b>, Contravariant<'c>);
    <'a, 'b, 'c> => (Contravariant<'a>, Invariant<'b>, Contravariant<'c>);
    <'a, 'b, 'c> => (Contravariant<'a>, Contravariant<'b>, Invariant<'c>);
    // Mixed: 2*Inv + 1*Contra
    <'a, 'b, 'c> => (Contravariant<'a>, Invariant<'b>, Invariant<'c>);
    <'a, 'b, 'c> => (Invariant<'a>, Contravariant<'b>, Invariant<'c>);
    <'a, 'b, 'c> => (Invariant<'a>, Invariant<'b>, Contravariant<'c>);

    // Mixed: 1*Co + 1*Contra
    <'a, 'b> => (Covariant<'a>, Contravariant<'b>);
    <'a, 'b> => (Contravariant<'a>, Covariant<'b>);
    // Mixed: 1*Co + 2*Contra
    <'a, 'b, 'c> => (Covariant<'a>, Contravariant<'b>, Contravariant<'c>);
    <'a, 'b, 'c> => (Contravariant<'a>, Covariant<'b>, Contravariant<'c>);
    <'a, 'b, 'c> => (Contravariant<'a>, Contravariant<'b>, Covariant<'c>);
    // Mixed: 2*Co + 1*Contra
    <'a, 'b, 'c> => (Contravariant<'a>, Covariant<'b>, Covariant<'c>);
    <'a, 'b, 'c> => (Covariant<'a>, Contravariant<'b>, Covariant<'c>);
    <'a, 'b, 'c> => (Covariant<'a>, Covariant<'b>, Contravariant<'c>);

    // Mixed: 1*inv + 1*Co + 1*Contra
    <'a, 'b, 'c> => (Invariant<'a>, Covariant<'b>, Contravariant<'c>);
    <'a, 'b, 'c> => (Invariant<'a>, Contravariant<'b>, Covariant<'c>);
    <'a, 'b, 'c> => (Covariant<'a>, Invariant<'b>, Contravariant<'c>);
    <'a, 'b, 'c> => (Contravariant<'a>, Invariant<'b>, Covariant<'c>);
    <'a, 'b, 'c> => (Covariant<'a>, Contravariant<'b>, Invariant<'c>);
    <'a, 'b, 'c> => (Contravariant<'a>, Covariant<'b>, Invariant<'c>);
}*/


/// Marker trait indicating that the implementing [`Transience`] can safely
/// "downgrade" to the `Other` one.
///
/// # SAFETY
/// This trait must only be implemented for *valid* conversions. Implementing
/// this trait for an *invalid* conversion (such as shortening the lifetime
/// of a *contravariant* type) can lead to undefined behavior.
pub unsafe trait IntoTransience<Other: ?Sized> {}


/// `Static` can safely converted to any other `Transience`.
unsafe impl<V: Transience + ?Sized> IntoTransience<V> for Static {}


/// `Covariant` can shorten its lifetime.
unsafe impl<'short, 'long: 'short>
    IntoTransience<Co<'short>> for Co<'long> {}

/// `Covariant` can shorten its lifetime and/or become `Invariant`.
unsafe impl<'short, 'long: 'short>
    IntoTransience<Inv<'short>> for Co<'long> {}


/// `Contravariant` can lengthen its lifetime.
unsafe impl<'short, 'long: 'short>
    IntoTransience<Contra<'long>> for Contra<'short> {}

/// `Contravariant` can lengthen its lifetime and/or become `Invariant`.
unsafe impl<'short, 'long: 'short>
    IntoTransience<Inv<'long>> for Contra<'short> {}


/// `Contravariant` can lengthen its lifetime.
unsafe impl<'a> IntoTransience<Inv<'a>> for Inv<'a> {}



unsafe impl<A1: ?Sized, A2: ?Sized>
    IntoTransience<(A2,)> for (A1,)
where
    // (A1,): Transience,
    A1: IntoTransience<A2>,
{}

unsafe impl<A1, B1: ?Sized, A2, B2: ?Sized>
    IntoTransience<(A2, B2)> for (A1, B1)
where
    // (A1, B1): Transience,
    A1: IntoTransience<A2>,
    B1: IntoTransience<B2>,
{}

unsafe impl<A1, B1, C1: ?Sized, A2, B2, C2: ?Sized>
    IntoTransience<(A2, B2, C2)> for (A1, B1, C1)
where
    // (A1, B1, C1): Transience,
    A1: IntoTransience<A2>,
    B1: IntoTransience<B2>,
    C1: IntoTransience<C2>,
{}

unsafe impl<A1, B1, C1, D1: ?Sized, A2, B2, C2, D2: ?Sized>
    IntoTransience<(A2, B2, C2, D2)> for (A1, B1, C1, D1)
where
    // (A1, B1, C1, D1): Transience,
    A1: IntoTransience<A2>,
    B1: IntoTransience<B2>,
    C1: IntoTransience<C2>,
    D1: IntoTransience<D2>,
{}
