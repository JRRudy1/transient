//! Ensure fails when the `covariant` attribute is used on a `'static` struct
use transient::Transient;


#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S {
    #[variance(unsafe_covariant)]
    value1: i32,
    value2: String,
}

fn main() {
    // this test should fail to compile
}
