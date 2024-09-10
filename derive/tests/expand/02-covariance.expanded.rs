//! Verifies that the `covariant` attribute expands as expected
use transient_derive::Transient;
struct NoGenerics {
    value1: String,
}
impl ::transient::Static for NoGenerics {}
struct TypeOnly<T> {
    value: T,
}
impl<T> ::transient::Static for TypeOnly<T>
where
    T: 'static,
{}
#[covariant(a)]
struct LifetimeOnly<'a> {
    value1: &'a str,
}
unsafe impl<'a> ::transient::Transient for LifetimeOnly<'a> {
    type Static = LifetimeOnly<'static>;
    type Transience = ::transient::Co<'a>;
}
const _: () = {
    mod validate_LifetimeOnly {
        #![allow(non_snake_case, dead_code)]
        use super::*;
        fn covariant_wrt_a<'__long, 'a>(v: LifetimeOnly<'__long>) -> LifetimeOnly<'a>
        where
            '__long: 'a,
        {
            v
        }
    }
};
#[covariant(a)]
struct TypeAndLifetime<'a, T> {
    value: &'a T,
}
unsafe impl<'a, T> ::transient::Transient for TypeAndLifetime<'a, T>
where
    T: 'static,
{
    type Static = TypeAndLifetime<'static, T>;
    type Transience = ::transient::Co<'a>;
}
const _: () = {
    mod validate_TypeAndLifetime {
        #![allow(non_snake_case, dead_code)]
        use super::*;
        fn covariant_wrt_a<'__long, 'a, T>(
            v: TypeAndLifetime<'__long, T>,
        ) -> TypeAndLifetime<'a, T>
        where
            T: 'static,
            '__long: 'a,
        {
            v
        }
    }
};
#[covariant(a)]
struct TypesAndLifetime<'a, T1, T2> {
    value1: &'a T1,
    value2: T2,
}
unsafe impl<'a, T1, T2> ::transient::Transient for TypesAndLifetime<'a, T1, T2>
where
    T1: 'static,
    T2: 'static,
{
    type Static = TypesAndLifetime<'static, T1, T2>;
    type Transience = ::transient::Co<'a>;
}
const _: () = {
    mod validate_TypesAndLifetime {
        #![allow(non_snake_case, dead_code)]
        use super::*;
        fn covariant_wrt_a<'__long, 'a, T1, T2>(
            v: TypesAndLifetime<'__long, T1, T2>,
        ) -> TypesAndLifetime<'a, T1, T2>
        where
            T1: 'static,
            T2: 'static,
            '__long: 'a,
        {
            v
        }
    }
};
#[covariant(a, b)]
struct TypesAndTwoLifetimes<'a, 'b, T1, T2: 'static> {
    value1: &'a T1,
    value2: &'a T2,
}
unsafe impl<'a, 'b, T1, T2: 'static> ::transient::Transient
for TypesAndTwoLifetimes<'a, 'b, T1, T2>
where
    T1: 'static,
{
    type Static = TypesAndTwoLifetimes<'static, 'static, T1, T2>;
    type Transience = (::transient::Co<'a>, ::transient::Co<'b>);
}
const _: () = {
    mod validate_TypesAndTwoLifetimes {
        #![allow(non_snake_case, dead_code)]
        use super::*;
        fn covariant_wrt_a<'__long, 'a, 'b, T1, T2: 'static>(
            v: TypesAndTwoLifetimes<'__long, 'b, T1, T2>,
        ) -> TypesAndTwoLifetimes<'a, 'b, T1, T2>
        where
            T1: 'static,
            '__long: 'a,
        {
            v
        }
        fn covariant_wrt_b<'__long, 'a, 'b, T1, T2: 'static>(
            v: TypesAndTwoLifetimes<'a, '__long, T1, T2>,
        ) -> TypesAndTwoLifetimes<'a, 'b, T1, T2>
        where
            T1: 'static,
            '__long: 'b,
        {
            v
        }
    }
};
