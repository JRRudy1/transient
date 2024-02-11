
use transient::Transient;

#[derive(Debug, Clone, PartialEq, Eq, Transient)]
struct S<'a, T1, T2> {
    borrowed: &'a T1,
    owned: T2,
}
type SS<'a> = S<'a, String, usize>;


fn main() {
    let (string, number) = ("qwer".to_string(), 5_usize);
    let mut original: SS = S{borrowed: &string, owned: number};
    { // mut ref
    let erased = original.erase_mut();
    assert_eq!(erased.type_id(), SS::static_type_id());
    let restored = erased.restore::<SS>().unwrap();
    assert_eq!(&*restored, &S{borrowed: &string, owned: number});
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