//! Ensure fails when the `covariant` attribute is used without `unsafe_`
use transient::Transient;


#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<'a, T> {
    #[variance(co)]
    value1: &'a T,
}

fn main() {
    // this test should fail to compile
}
