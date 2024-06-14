//! Tests the behavior when used on structs with two type parameters
use transient::{Transient, AnyOps};

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<'a, T1, T2> {
    borrowed: &'a T1,
    owned: T2,
}
type SS<'a> = S<'a, String, usize>;


fn main() {
    let (string, number) = ("qwer".to_string(), 5_usize);
    let mut original: SS = S{borrowed: &string, owned: number};
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