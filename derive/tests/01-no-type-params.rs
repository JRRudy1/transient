
use transient_any::{Erase, Erased, ErasedRef, ErasedMut};
use transient_any_derive::MakeStatic;


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
    let original: SS<'_> = SS{value: &string };
    let erased: Erased<'_> = original.clone().into_erased();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, original);
}

fn test_ref() {
    let string = "qwer".to_string();
    let original: SS<'_> = SS{value: &string };
    let erased: ErasedRef<'_> = original.as_erased();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
}

fn test_mut() {
    let string = "qwer".to_string();
    let mut original: SS<'_> = SS{value: &string };
    let erased: ErasedMut = original.as_erased_mut();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored.value, &string);
}
