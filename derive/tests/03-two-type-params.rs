
use transient_any::{Erase, Erased, ErasedRef, ErasedMut};
use transient_any_derive::MakeStatic;


#[derive(Debug, Clone, PartialEq, Eq, MakeStatic)]
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
    let original: SS<'_> = S{borrowed: &string, owned: number};
    let erased: Erased<'_> = original.into_erased();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored.borrowed, &string);
    assert_eq!(restored.owned, number);
}

fn test_ref() { // single lifetime (derived `MakeStatic` impl)
    let value = "qwer".to_string();
    let original: SS<'_> = S{borrowed: &value, owned: 5};
    let erased: ErasedRef<'_> = original.as_erased();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
}

fn test_mut() {
    let (string, number) = ("qwer".to_string(), 5_usize);
    let mut original: SS<'_> = S{borrowed: &string, owned: number};
    let erased: ErasedMut = original.as_erased_mut();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored.borrowed, &string);
    assert_eq!(restored.owned, number);
}
