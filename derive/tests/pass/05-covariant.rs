//! Tests the behavior when used on structs with no type parameters
use transient::{Any, Co, Downcast, Inv, Transient};

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
#[variance('a = covariant)]
struct SS<'a> {
    value: &'a String,
}

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
#[variance('a = cov)]
struct _SS<'a> {
    value: &'a String,
}

fn main() {
    let string = "qwer".to_string();
    let original = SS { value: &string };
    let inv_erased = &original as &dyn Any<Inv>;
    assert_eq!(inv_erased.downcast_ref::<SS>(), Some(&original));

    let co_erased = &original as &dyn Any<Co>;
    assert_eq!(co_erased.downcast_ref::<SS>(), Some(&original));
}
