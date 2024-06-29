//! Verifies that the macro expands as expected with no attributes
use transient_derive::Transient;

#[derive(Transient)]
struct NoGenerics {
    value1: String,
}

#[derive(Transient)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}

#[derive(Transient)]
struct TypeOnly<T> {
    value: T,
}

#[derive(Transient)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}

#[derive(Transient)]
struct TypesAndLifetime<'a, T1, T2: 'static> {
    value1: &'a T1,
    value2: T2,
}
