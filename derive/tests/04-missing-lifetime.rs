

//! Tests the behavior when used on structs without a lifetime parameter
use transient_any_derive::MakeStatic;


#[derive(Debug, Clone, PartialEq, Eq, MakeStatic)]
struct S<T> {
    value: T,
}

type SS = S<String>;


fn main() {
    // this test should fail to compile
}
