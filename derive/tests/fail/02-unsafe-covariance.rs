//! Ensure fails when the `covariant` attribute is used without `r#unsafe`
use transient::Transient;


#[derive(Debug, Clone, PartialEq, Eq, Transient)]
#[covariant]
struct S<'a, 'b, T> {
    value1: &'a T,
    value2: &'b T,
}

fn main() {
    // this test should fail to compile
}
