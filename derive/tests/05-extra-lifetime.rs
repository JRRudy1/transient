//! Tests the behavior when used on structs with too many lifetime parameters
use transient_any::MakeStatic;


#[derive(Debug, Clone, PartialEq, Eq, MakeStatic)]
struct S<'a, 'b, T> {
    value1: &'a T,
    value2: &'b T,
}


fn main() {
    // this test should fail to compile
}
