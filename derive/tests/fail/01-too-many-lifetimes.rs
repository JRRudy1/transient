//! Tests the behavior when used on structs with too many lifetime parameters
use transient::Transient;

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<'a, 'b, 'c, 'd, 'e, 'f, T> {
    value1: &'a T,
    value2: &'b T,
    value3: &'c T,
    value4: &'d T,
    value5: &'e T,
    value6: &'f T,
}

fn main() {
    // this test should fail to compile
}
