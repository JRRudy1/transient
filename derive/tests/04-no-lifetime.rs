//! Tests the behavior when used on structs without a lifetime parameter
use transient_any::{Erased, ErasedRef, ErasedMut, TransientAny};


#[derive(Debug, Clone, PartialEq, Eq, TransientAny)]
struct S<T> {
    value: T,
}

type SS = S<String>;


fn main() {
    test_owned();
    test_ref();
    test_mut();
}

fn test_owned() {
    let original = S{value: "qwer".to_string()};
    let erased: Erased<'static> = original.clone().into_erased();
    assert_eq!(erased.type_id(), SS::type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, original);
}

fn test_ref() {
    let original = S{value: "qwer".to_string()};
    let erased: ErasedRef<'_, 'static> = original.as_erased();
    assert_eq!(erased.type_id(), SS::type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
}

fn test_mut() {
    let mut original = S{value: "qwer".to_string()};
    let erased: ErasedMut<'_, 'static> = original.as_erased_mut();
    assert_eq!(erased.type_id(), SS::type_id());
    let restored = erased.restore::<SS>().unwrap().clone();
    assert_eq!(restored, original);
}