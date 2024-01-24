
use transient_any::{Erase, Erased, ErasedRef, ErasedMut};
use transient_any_derive::MakeStatic;


#[derive(Debug, Clone, PartialEq, Eq, MakeStatic)]
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
    let original: SS<'_> = S{value: &string };
    let erased: Erased<'_> = original.clone().into_erased();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, original);
}

fn test_ref() { // single lifetime (derived `MakeStatic` impl)
    let string = "qwer".to_string();
    let original: SS<'_> = S{value: &string};
    let erased: ErasedRef<'_> = original.as_erased();
    erased.assert_is::<SS>();
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
}

fn test_mut() { // start of 'real
    let string = "qwer".to_string();                 // lifetime = 'real
    let mut original: SS<'_> = SS{value: &string};         // lifetime <= 'real

    let restored = { // start of 'erased
        let erased: ErasedMut<'_, '_> = original.as_erased_mut();  // &mut: 'erased
        erased.assert_is::<SS>();
        erased.restore::<SS>().unwrap().clone()  // borrow should end?
    }; // end of 'erased

    // let erased: ErasedMut<'_> = original.as_erased_mut();  // mutable borrow starts
    // erased.assert_is::<SS>();
    // let restored = erased.restore::<SS>().unwrap().clone();  // borrow should end?

    assert_eq!(restored, original);
} // end of 'real
