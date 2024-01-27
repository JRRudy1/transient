
use transient_any::{Erased, ErasedRef, ErasedMut, TransientAny};


#[derive(Debug, Clone, PartialEq, Eq, TransientAny)]
struct S<'a, T> {
    value: &'a T,
}

type SS<'a> = S<'a, String>;


fn main() {
    test_owned();
    test_ref();
    test_mut();
}

fn test_owned() {
    let string = "qwer".to_string();
    let original: SS = S{value: &string };
    let erased: Erased = original.clone().erase();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, original);
}

fn test_ref() {
    let string = "qwer".to_string();
    let original: SS = S{value: &string};
    let erased: ErasedRef = original.erase_ref();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
}

fn test_mut() {
    let string = "qwer".to_string();
    let mut original: SS = SS{value: &string};
    let erased: ErasedMut = original.erase_mut();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap().clone();
    assert_eq!(restored, original);
}
