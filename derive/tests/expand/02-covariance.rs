//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;

#[derive(Transient)]
struct NoGenerics {
    value1: String,
}

#[derive(Transient)]
struct TypeOnly<T> {
    value: T,
}

#[derive(Transient)]
#[covariant(a)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}

#[derive(Transient)]
#[covariant(a)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}

#[derive(Transient)]
#[covariant(a)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}

#[derive(Transient)]
#[covariant(a, b)]
struct TypesAndTwoLifetimes<'a, 'b, T1, T2: 'static> {
    value1: &'a T1,
    value2: &'a T2,
}
