//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;

#[derive(Transient)]
struct NoGenerics {
    value1: String,
}

#[derive(Transient)]
#[variance('a = covariant)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}

#[derive(Transient)]
struct TypeOnly<T> {
    value: T,
}

#[derive(Transient)]
#[variance('a = covariant)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}

#[derive(Transient)]
#[variance('a = covariant)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
