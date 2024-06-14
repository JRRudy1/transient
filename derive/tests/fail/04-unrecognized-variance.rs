//! Ensure fails when an unrecognized variance is requested
use transient::Transient;


#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<'a, T> {
    #[variance(invarient)]
    value1: &'a T,
}

fn main() {
    // this test should fail to compile
}
