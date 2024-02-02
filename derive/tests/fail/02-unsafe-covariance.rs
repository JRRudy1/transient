//! Ensure fails when the `covariant` attribute is used without `r#unsafe`
use transient_any::TransientAny;


#[derive(Debug, Clone, PartialEq, Eq, TransientAny)]
#[covariant]
struct S<'a, 'b, T> {
    value1: &'a T,
    value2: &'b T,
}

fn main() {
    // this test should fail to compile
}
