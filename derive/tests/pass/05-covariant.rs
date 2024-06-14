//! Tests the behavior when used on structs with no type parameters
use transient::{Transient, Any, Downcast, Transcend, Co, Inv};

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct SS<'a> {
    #[variance(unsafe_covariant)]
    value: &'a String,
}

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct _SS<'a> {
    #[variance(unsafe_co)]
    value: &'a String,
}

fn main() {
    let string = "qwer".to_string();
    let original = SS{value: &string};
    let inv_erased = &original as &dyn Any<Inv>;
    assert_eq!(inv_erased.downcast_ref::<SS>(), Some(&original));

    let co_erased = &original as &dyn Any<Co>;
    assert_eq!(co_erased.downcast_ref::<SS>(), Some(&original));

    let tr_erased: &dyn Any<Inv> = co_erased.transcend_ref();
    assert_eq!(tr_erased.downcast_ref::<SS>(), Some(&original));
}
