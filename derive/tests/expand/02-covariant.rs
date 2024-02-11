//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;

#[derive(Transient)] // future fail
#[r#unsafe(covariant)]
struct NoGenerics {
    value1: String,
}

#[derive(Transient)]
#[r#unsafe(covariant)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}

#[derive(Transient)] // future fail
#[r#unsafe(covariant)]
struct TypeOnly<T> {
    value: T,
}

#[derive(Transient)]
#[r#unsafe(covariant)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}

#[derive(Transient)] // future fail
#[r#unsafe(covariant)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
