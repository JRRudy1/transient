
use transient_any::{Erased, ErasedRef, ErasedMut, TransientAny};


#[derive(Debug, Clone, PartialEq, Eq, TransientAny)]
struct S<'a, T1, T2> {
    borrowed: &'a T1,
    owned: T2,
}

type SS<'a> = S<'a, String, usize>;


fn main() {
    test_owned();
    test_ref();
    test_mut();
}

fn test_owned() {
    let (string, number) = ("qwer".to_string(), 5_usize);
    let original: SS = S{borrowed: &string, owned: number};
    let erased: Erased = original.erase();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored.borrowed, &string);
    assert_eq!(restored.owned, number);
}

fn test_ref() { // single lifetime (derived `TransientAny` impl)
    let value = "qwer".to_string();
    let original: SS = S{borrowed: &value, owned: 5};
    let erased: ErasedRef = original.erase_ref();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
}

fn test_mut() {
    let (string, number) = ("qwer".to_string(), 5_usize);
    let mut original: SS = S{borrowed: &string, owned: number};
    let erased: ErasedMut = original.erase_mut();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored.borrowed, &string);
    assert_eq!(restored.owned, number);
}
