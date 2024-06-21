//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;
struct LifetimeOnly<'a> {
    #[variance(unsafe_covariant)]
    value1: &'a str,
}
unsafe impl<'a> ::transient::Transient for LifetimeOnly<'a> {
    type Static = LifetimeOnly<'static>;
    type Transience = ::transient::Co<'a>;
}
struct TypeAndLifetime<'a, T> {
    #[variance(unsafe_covariant)]
    value: &'a T,
}
unsafe impl<'a, T: 'static> ::transient::Transient for TypeAndLifetime<'a, T> {
    type Static = TypeAndLifetime<'static, T>;
    type Transience = ::transient::Co<'a>;
}
struct TypesAndLifetime<'a, T1, T2> {
    #[variance(unsafe_covariant)]
    value1: &'a T1,
    value2: T2,
}
unsafe impl<'a, T1: 'static, T2: 'static> ::transient::Transient
for TypesAndLifetime<'a, T1, T2> {
    type Static = TypesAndLifetime<'static, T1, T2>;
    type Transience = ::transient::Co<'a>;
}
