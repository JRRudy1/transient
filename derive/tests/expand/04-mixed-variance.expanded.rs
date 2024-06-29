//! Verifies that the mixed variance attributes expands as expected
use transient_derive::Transient;
#[contravariant(a)]
#[covariant(b)]
struct ContraCo<'a, 'b, T> {
    value1: fn(&'a T) -> &'b str,
}
unsafe impl<'a, 'b, T> ::transient::Transient for ContraCo<'a, 'b, T>
where
    T: 'static,
{
    type Static = ContraCo<'static, 'static, T>;
    type Transience = (::transient::Contra<'a>, ::transient::Co<'b>);
}
mod __validate_ContraCo {
    #![allow(non_snake_case, dead_code)]
    use super::*;
    fn contravariant_wrt_a<'__short, 'a, 'b, T>(
        v: ContraCo<'__short, 'b, T>,
    ) -> ContraCo<'a, 'b, T>
    where
        T: 'static,
        'a: '__short,
    {
        v
    }
    fn covariant_wrt_b<'__long, 'a, 'b, T>(
        v: ContraCo<'a, '__long, T>,
    ) -> ContraCo<'a, 'b, T>
    where
        T: 'static,
        '__long: 'b,
    {
        v
    }
}
#[covariant(a)]
struct CoInv<'a, 'b, T1, T2> {
    value1: &'a T1,
    value2: *mut T2,
}
unsafe impl<'a, 'b, T1, T2> ::transient::Transient for CoInv<'a, 'b, T1, T2>
where
    T1: 'static,
    T2: 'static,
{
    type Static = CoInv<'static, 'static, T1, T2>;
    type Transience = (::transient::Co<'a>, ::transient::Inv<'b>);
}
mod __validate_CoInv {
    #![allow(non_snake_case, dead_code)]
    use super::*;
    fn covariant_wrt_a<'__long, 'a, 'b, T1, T2>(
        v: CoInv<'__long, 'b, T1, T2>,
    ) -> CoInv<'a, 'b, T1, T2>
    where
        T1: 'static,
        T2: 'static,
        '__long: 'a,
    {
        v
    }
}
#[covariant]
struct GlobalCo<'a, 'b, T1, T2> {
    value1: &'a T1,
    value2: &'b T2,
}
unsafe impl<'a, 'b, T1, T2> ::transient::Transient for GlobalCo<'a, 'b, T1, T2>
where
    T1: 'static,
    T2: 'static,
{
    type Static = GlobalCo<'static, 'static, T1, T2>;
    type Transience = (::transient::Co<'a>, ::transient::Co<'b>);
}
mod __validate_GlobalCo {
    #![allow(non_snake_case, dead_code)]
    use super::*;
    fn covariant_wrt_a<'__long, 'a, 'b, T1, T2>(
        v: GlobalCo<'__long, 'b, T1, T2>,
    ) -> GlobalCo<'a, 'b, T1, T2>
    where
        T1: 'static,
        T2: 'static,
        '__long: 'a,
    {
        v
    }
    fn covariant_wrt_b<'__long, 'a, 'b, T1, T2>(
        v: GlobalCo<'a, '__long, T1, T2>,
    ) -> GlobalCo<'a, 'b, T1, T2>
    where
        T1: 'static,
        T2: 'static,
        '__long: 'b,
    {
        v
    }
}
