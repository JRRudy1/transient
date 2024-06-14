//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;

#[derive(Transient)] // future fail
struct NoGenerics {
    #[variance(unsafe_covariant)]
    value1: String,
}

#[derive(Transient)]
struct LifetimeOnly<'a> {
    #[variance(unsafe_covariant)]
    value1: &'a str,
}

#[derive(Transient)] // future fail
struct TypeOnly<T> {
    #[variance(unsafe_covariant)]
    value: T,
}

#[derive(Transient)]
struct TypeAndLifetime<'a, T> {
    #[variance(unsafe_covariant)]
    value: &'a T,
}

#[derive(Transient)]
struct TypesAndLifetime<'a, T1, T2> {
    #[variance(unsafe_covariant)]
    value1: &'a T1,
    value2: T2,
}
