//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;
#[r#unsafe(covariant)]
struct NoGenerics {
    value1: String,
}
unsafe impl transient::Transient for NoGenerics {
    type Static = NoGenerics;
    type Transience = transient::Static;
}
#[r#unsafe(covariant)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}
unsafe impl<'a> transient::Transient for LifetimeOnly<'a> {
    type Static = LifetimeOnly<'static>;
    type Transience = transient::Covariant<'a>;
}
#[r#unsafe(covariant)]
struct TypeOnly<T> {
    value: T,
}
unsafe impl<T: 'static> transient::Transient for TypeOnly<T> {
    type Static = TypeOnly<T>;
    type Transience = transient::Static;
}
#[r#unsafe(covariant)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}
unsafe impl<'a, T: 'static> transient::Transient for TypeAndLifetime<'a, T> {
    type Static = TypeAndLifetime<'static, T>;
    type Transience = transient::Covariant<'a>;
}
#[r#unsafe(covariant)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
unsafe impl<'a, T1: 'static, T2: 'static> transient::Transient
for TypesAndLifetime<'a, T1, T2> {
    type Static = TypesAndLifetime<'static, T1, T2>;
    type Transience = transient::Covariant<'a>;
}
