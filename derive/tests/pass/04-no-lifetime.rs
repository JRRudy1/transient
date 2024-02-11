//! Tests the behavior when used on structs without a lifetime parameter
use transient::Transient;


#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<T> {
    value: T,
}

type SS = S<String>;

fn main() {
    let mut original = S{value: "qwer".to_string()};
    { // mut ref
    let erased = original.erase_mut();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(&*restored, &S{value: "qwer".to_string()});
    } { // shared ref
    let erased = original.erase_ref();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, &original);
    } { // owned
    let erased = original.clone().erase();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored, original);
    }
}
