//! Ensure fails when an unrecognized variance is requested
use transient::Transient;

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
#[cvariant(a)]
struct S<'a, T> {
    value1: &'a T,
}

fn main() {
    // this test should fail to compile
}
