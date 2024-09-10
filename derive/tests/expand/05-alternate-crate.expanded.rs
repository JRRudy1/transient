//! Verifies that the mixed variance attributes expands as expected
use transient_derive::Transient;
#[transient(crate = mycrate::transient)]
#[contravariant(a)]
#[covariant(b)]
struct ContraCo<'a, 'b, T> {
    value1: fn(&'a T) -> &'b str,
}
unsafe impl<'a, 'b, T> mycrate::transient::Transient for ContraCo<'a, 'b, T>
where
    T: 'static,
{
    type Static = ContraCo<'static, 'static, T>;
    type Transience = (mycrate::transient::Contra<'a>, mycrate::transient::Co<'b>);
}
const _: () = {
    mod validate_ContraCo {
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
};
