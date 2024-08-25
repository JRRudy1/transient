//! Verifies that the mixed variance attributes expands as expected
use transient_derive::Transient;

#[derive(Transient)]
#[transient(crate = mycrate::transient)]
#[contravariant(a)]
#[covariant(b)]
struct ContraCo<'a, 'b, T> {
    value1: fn(&'a T) -> &'b str,
}
