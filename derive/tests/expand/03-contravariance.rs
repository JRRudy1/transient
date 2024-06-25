//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;

#[derive(Transient)]
#[contravariant(a)]
struct LifetimeOnly<'a> {
    value1: fn(&'a str),
}

#[derive(Transient)]
#[contravariant(a)]
struct TypeAndLifetime<'a, T> {
    value: fn(&'a T),
}

#[derive(Transient)]
#[contravariant(a)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: fn(&'a T1) -> T2,
}

#[derive(Transient)]
#[contravariant(a, b)]
struct TypesAndTwoLifetimes<'a, 'b, T1, T2> {
    value1: fn(&'a T1),
    value2: fn(&'a T2),
}
