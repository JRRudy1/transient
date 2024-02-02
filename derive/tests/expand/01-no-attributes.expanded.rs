//! Verifies that the macro expands as expected with no attributes
use transient_any_derive::TransientAny;
struct NoGenerics {
    value1: String,
}
unsafe impl transient_any::TransientAny<'static> for NoGenerics {
    type Static = NoGenerics;
    type Variance = transient_any::Static;
}
struct LifetimeOnly<'a> {
    value1: &'a str,
}
unsafe impl<'a> transient_any::TransientAny<'a> for LifetimeOnly<'a> {
    type Static = LifetimeOnly<'static>;
    type Variance = transient_any::Invariant<'a>;
}
struct TypeOnly<T> {
    value: T,
}
unsafe impl<T: 'static> transient_any::TransientAny<'static> for TypeOnly<T> {
    type Static = TypeOnly<T>;
    type Variance = transient_any::Static;
}
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}
unsafe impl<'a, T: 'static> transient_any::TransientAny<'a> for TypeAndLifetime<'a, T> {
    type Static = TypeAndLifetime<'static, T>;
    type Variance = transient_any::Invariant<'a>;
}
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
unsafe impl<'a, T1: 'static, T2: 'static> transient_any::TransientAny<'a>
for TypesAndLifetime<'a, T1, T2> {
    type Static = TypesAndLifetime<'static, T1, T2>;
    type Variance = transient_any::Invariant<'a>;
}
