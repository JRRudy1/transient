//! Verifies that the macro expands as expected with no attributes
use transient_derive::Transient;
struct NoGenerics {
    value1: String,
}
impl ::transient::Static for NoGenerics {}
struct LifetimeOnly<'a> {
    value1: &'a str,
}
unsafe impl<'a> ::transient::Transient for LifetimeOnly<'a> {
    type Static = LifetimeOnly<'static>;
    type Transience = ::transient::Inv<'a>;
}
struct TypeOnly<T> {
    value: T,
}
impl<T> ::transient::Static for TypeOnly<T>
where
    T: 'static,
{}
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}
unsafe impl<'a, T> ::transient::Transient for TypeAndLifetime<'a, T>
where
    T: 'static,
{
    type Static = TypeAndLifetime<'static, T>;
    type Transience = ::transient::Inv<'a>;
}
struct TypesAndLifetime<'a, T1, T2: 'static> {
    value1: &'a T1,
    value2: T2,
}
unsafe impl<'a, T1, T2: 'static> ::transient::Transient for TypesAndLifetime<'a, T1, T2>
where
    T1: 'static,
{
    type Static = TypesAndLifetime<'static, T1, T2>;
    type Transience = ::transient::Inv<'a>;
}
