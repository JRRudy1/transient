//! Verifies that the macro expands as expected with no attributes
use transient_any_derive::TransientAny;

#[derive(TransientAny)]
struct NoGenerics {
    value1: String,
}

#[derive(TransientAny)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}

#[derive(TransientAny)]
struct TypeOnly<T> {
    value: T,
}

#[derive(TransientAny)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}

#[derive(TransientAny)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
