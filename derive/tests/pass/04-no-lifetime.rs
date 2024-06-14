//! Tests the behavior when used on structs without a lifetime parameter
use transient::{Transient, Downcast};

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<T> {
    value: T,
}

type SS = S<String>;

fn main() {
    let mut original = S{value: "qwer".to_string()};
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
