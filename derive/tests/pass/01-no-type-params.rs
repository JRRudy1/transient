
use transient_any::{TransientAny};

#[derive(Debug, Clone, PartialEq, Eq, TransientAny)]
struct SS<'a> {
    value: &'a String,
}

fn main() {
    let string = "qwer".to_string();
    let mut original = SS{value: &string};
    { // mut ref
    let erased = original.erase_mut();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(restored.value, &string);
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
