//! Verifies that the mixed variance attributes expands as expected
use transient_derive::Transient;
#[contravariant(a)]
#[covariant(b)]
struct ContraCo<'a, 'b, T> {
    value1: fn(&'a T) -> &'b str,
}
unsafe impl<'a, 'b, T: 'static> ::transient::Transient for ContraCo<'a, 'b, T> {
    type Static = ContraCo<'static, 'static, T>;
    type Transience = (::transient::Contra<'a>, ::transient::Co<'b>);
}
#[allow(non_snake_case, dead_code)]
mod __validate_ContraCo {
    use super::*;
    fn contravariant_wrt_a<'__short, 'a, 'b, T: 'static>(
        v: ContraCo<'__short, 'b, T>,
    ) -> ContraCo<'a, 'b, T>
    where
        'a: '__short,
    {
        v
    }
    fn covariant_wrt_b<'__long, 'a, 'b, T: 'static>(
        v: ContraCo<'a, '__long, T>,
    ) -> ContraCo<'a, 'b, T>
    where
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
unsafe impl<'a, 'b, T1: 'static, T2: 'static> ::transient::Transient for CoInv<'a, 'b, T1, T2> {
    type Static = CoInv<'static, 'static, T1, T2>;
    type Transience = (::transient::Co<'a>, ::transient::Inv<'b>);
}
#[allow(non_snake_case, dead_code)]
mod __validate_CoInv {
    use super::*;
    fn covariant_wrt_a<'__long, 'a, 'b, T1: 'static, T2: 'static>(
        v: CoInv<'__long, 'b, T1, T2>,
    ) -> CoInv<'a, 'b, T1, T2>
    where
        '__long: 'a,
    {
        v
    }
}
