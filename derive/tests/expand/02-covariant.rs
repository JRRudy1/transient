//! Verifies that the `covariant` attribute expands as expected
use transient_any_derive::TransientAny;

#[derive(TransientAny)] // future fail
#[r#unsafe(covariant)]
struct NoGenerics {
    value1: String,
}

#[derive(TransientAny)]
#[r#unsafe(covariant)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}

#[derive(TransientAny)] // future fail
#[r#unsafe(covariant)]
struct TypeOnly<T> {
    value: T,
}

#[derive(TransientAny)]
#[r#unsafe(covariant)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}

#[derive(TransientAny)] // future fail
#[r#unsafe(covariant)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
