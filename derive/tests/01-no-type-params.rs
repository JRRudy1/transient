
use transient_any::{Erased, ErasedRef, ErasedMut, Erase, MakeStatic};


#[derive(Debug, Clone, PartialEq, Eq, MakeStatic)]
struct SS<'a> {
    value: &'a String,
}


fn main() {
    test_owned();
    test_ref();
    test_mut();
}

fn test_owned() {
    let string = "qwer".to_string();
    let original: SS = SS{value: &string };
    let erased: Erased = original.clone().into_erased();
    assert_eq!(erased.type_id(), SS::type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, original);
}

fn test_ref() {
    let string = "qwer".to_string();
    let original: SS = SS{value: &string };
    let erased: ErasedRef = original.as_erased();
    assert_eq!(erased.type_id(), SS::type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
}

fn test_mut() {
    let string = "qwer".to_string();
    let mut original: SS = SS{value: &string };
    let erased: ErasedMut = original.as_erased_mut();
    assert_eq!(erased.type_id(), SS::type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored.value, &string);
}