//! Verifies that the mixed variance attributes expands as expected
use transient_derive::Transient;

#[derive(Transient)]
#[contravariant(a)]
#[covariant(b)]
struct ContraCo<'a, 'b, T> {
    value1: fn(&'a T) -> &'b str,
}

#[derive(Transient)]
#[covariant(a)]
struct CoInv<'a, 'b, T1, T2> {
    value1: &'a T1,
    value2: *mut T2,
}

#[derive(Transient)]
#[covariant]
struct GlobalCo<'a, 'b, T1, T2> {
    value1: &'a T1,
    value2: &'b T2,
}
