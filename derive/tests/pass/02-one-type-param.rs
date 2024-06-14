//! Tests the behavior when used on structs with one type parameter
use transient::{Transient, Downcast};

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<'a, T> {
    value: &'a T,
}

type SS<'a> = S<'a, String>;

fn main() {
    let string = "qwer".to_string();
    let mut original = SS{value: &string};
    { // owned
        let erased = Box::new(original.clone()).erase();
        assert!(erased.is::<SS>());
        let restored = erased.downcast::<SS>().unwrap();
        assert_eq!(restored.as_ref(), &original);
    }
    { // shared ref
        let erased = original.erase_ref();
        assert!(erased.is::<SS>());
        let restored = erased.downcast_ref::<SS>().unwrap();
        assert_eq!(restored, &original);
    }
    { // mut ref
        let mut cloned = original.clone();
        let erased = cloned.erase_mut();
        assert!(erased.is::<SS>());
        let restored = erased.downcast_mut::<SS>().unwrap();
        assert_eq!(restored, &mut original);
    }
}
