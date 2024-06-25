//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;
#[contravariant(a)]
struct LifetimeOnly<'a> {
    value1: fn(&'a str),
}
unsafe impl<'a> ::transient::Transient for LifetimeOnly<'a> {
    type Static = LifetimeOnly<'static>;
    type Transience = ::transient::Contra<'a>;
}
#[allow(non_snake_case, dead_code)]
mod __validate_LifetimeOnly {
    use super::*;
    fn contravariant_wrt_a<'__short, 'a>(v: LifetimeOnly<'__short>) -> LifetimeOnly<'a>
    where
        'a: '__short,
    {
        v
    }
}
#[contravariant(a)]
struct TypeAndLifetime<'a, T> {
    value: fn(&'a T),
}
unsafe impl<'a, T: 'static> ::transient::Transient for TypeAndLifetime<'a, T> {
    type Static = TypeAndLifetime<'static, T>;
    type Transience = ::transient::Contra<'a>;
}
#[allow(non_snake_case, dead_code)]
mod __validate_TypeAndLifetime {
    use super::*;
    fn contravariant_wrt_a<'__short, 'a, T: 'static>(
        v: TypeAndLifetime<'__short, T>,
    ) -> TypeAndLifetime<'a, T>
    where
        'a: '__short,
    {
        v
    }
}
#[contravariant(a)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: fn(&'a T1) -> T2,
}
unsafe impl<'a, T1: 'static, T2: 'static> ::transient::Transient for TypesAndLifetime<'a, T1, T2> {
    type Static = TypesAndLifetime<'static, T1, T2>;
    type Transience = ::transient::Contra<'a>;
}
#[allow(non_snake_case, dead_code)]
mod __validate_TypesAndLifetime {
    use super::*;
    fn contravariant_wrt_a<'__short, 'a, T1: 'static, T2: 'static>(
        v: TypesAndLifetime<'__short, T1, T2>,
    ) -> TypesAndLifetime<'a, T1, T2>
    where
        'a: '__short,
    {
        v
    }
}
#[contravariant(a, b)]
struct TypesAndTwoLifetimes<'a, 'b, T1, T2> {
    value1: fn(&'a T1),
    value2: fn(&'a T2),
}
unsafe impl<'a, 'b, T1: 'static, T2: 'static> ::transient::Transient
    for TypesAndTwoLifetimes<'a, 'b, T1, T2>
{
    type Static = TypesAndTwoLifetimes<'static, 'static, T1, T2>;
    type Transience = (::transient::Contra<'a>, ::transient::Contra<'b>);
}
#[allow(non_snake_case, dead_code)]
mod __validate_TypesAndTwoLifetimes {
    use super::*;
    fn contravariant_wrt_a<'__short, 'a, 'b, T1: 'static, T2: 'static>(
        v: TypesAndTwoLifetimes<'__short, 'b, T1, T2>,
    ) -> TypesAndTwoLifetimes<'a, 'b, T1, T2>
    where
        'a: '__short,
    {
        v
    }
    fn contravariant_wrt_b<'__short, 'a, 'b, T1: 'static, T2: 'static>(
        v: TypesAndTwoLifetimes<'a, '__short, T1, T2>,
    ) -> TypesAndTwoLifetimes<'a, 'b, T1, T2>
    where
        'b: '__short,
    {
        v
    }
}
