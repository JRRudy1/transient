//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;
struct NoGenerics {
    value1: String,
}
unsafe impl ::transient::Transient for NoGenerics {
    type Static = NoGenerics;
    type Transience = ();
}
#[allow(unused)]
#[allow(non_snake_case)]
fn __validate_NoGenerics() {}
#[variance('a = covariant)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}
unsafe impl<'a> ::transient::Transient for LifetimeOnly<'a> {
    type Static = LifetimeOnly<'static>;
    type Transience = (::transient::Co<'a>);
}
#[allow(unused)]
#[allow(non_snake_case)]
fn __validate_LifetimeOnly() {
    #[allow(unused)]
    #[allow(non_snake_case)]
    fn __validate_LifetimeOnly_a<'__test_lifetime: 'a, 'a>(
        v: LifetimeOnly<'__test_lifetime>,
    ) -> LifetimeOnly<'a> {
        v
    }
}
struct TypeOnly<T> {
    value: T,
}
unsafe impl<T: 'static> ::transient::Transient for TypeOnly<T> {
    type Static = TypeOnly<T>;
    type Transience = ();
}
#[allow(unused)]
#[allow(non_snake_case)]
fn __validate_TypeOnly() {}
#[variance('a = covariant)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}
unsafe impl<'a, T: 'static> ::transient::Transient for TypeAndLifetime<'a, T> {
    type Static = TypeAndLifetime<'static, T>;
    type Transience = (::transient::Co<'a>);
}
#[allow(unused)]
#[allow(non_snake_case)]
fn __validate_TypeAndLifetime() {
    #[allow(unused)]
    #[allow(non_snake_case)]
    fn __validate_TypeAndLifetime_a<'__test_lifetime: 'a, 'a, T: 'static>(
        v: TypeAndLifetime<'__test_lifetime, T>,
    ) -> TypeAndLifetime<'a, T> {
        v
    }
}
#[variance('a = covariant)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
unsafe impl<'a, T1: 'static, T2: 'static> ::transient::Transient
for TypesAndLifetime<'a, T1, T2> {
    type Static = TypesAndLifetime<'static, T1, T2>;
    type Transience = (::transient::Co<'a>);
}
#[allow(unused)]
#[allow(non_snake_case)]
fn __validate_TypesAndLifetime() {
    #[allow(unused)]
    #[allow(non_snake_case)]
    fn __validate_TypesAndLifetime_a<'__test_lifetime: 'a, 'a, T1: 'static, T2: 'static>(
        v: TypesAndLifetime<'__test_lifetime, T1, T2>,
    ) -> TypesAndLifetime<'a, T1, T2> {
        v
    }
}
