
use super::{
    Erased, ErasedRef, ErasedMut,
    Erase, MakeStatic, derive_MakeStatic
};


#[derive(Debug, Clone, PartialEq, Eq)]
struct S<'a> {
    value: &'a String
}
derive_MakeStatic!{S<'a>}

// unsafe impl<'a> MakeStatic<'a> for S<'a> {
//     type Static = S<'static>;
// }


#[derive(Debug, Clone, PartialEq, Eq)]
struct S2<'a, 'b: 'a> {
    short: &'a String,
    long: &'b String,
}
unsafe impl<'a, 'b: 'a> MakeStatic<'a> for S2<'a, 'b> {
    type Static = S2<'static, 'static>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct S3<'a, T: 'static> {
    value: &'a T,
}
unsafe impl<'a, T: 'static> MakeStatic<'a> for S3<'a, T> {
    type Static = S3<'static, T>;
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct S4<'a, T> {
    value: &'a T,
}



#[cfg_attr(test, test)]
pub(super) fn test_ref() { // single lifetime (derived `MakeStatic` impl)
    let value = "qwer".to_string();
    let original: S<'_> = S{value: &value};
    let erased: ErasedRef<'_> = original.as_erased();
    erased.assert_is::<S>();
    let restored = erased.restore::<S>().unwrap();
    assert_eq!(restored, &original);
}
#[cfg_attr(test, test)]
pub(super) fn test_custom() { // multiple lifetimes (custom `MakeStatic` impl)
    let (short, long) = ("short".to_string(), "long".to_string());
    let original: S2<'_, '_> = S2{short: &short, long: &long};
    let erased: ErasedRef<'_> = original.as_erased();
    erased.assert_is::<S2>();
    let restored = erased.restore::<S2>().unwrap();
    assert_eq!(restored, &original);
}
#[cfg_attr(test, test)]
pub(super) fn test_generic() { // single lifetime, generic over `T` (custom `MakeStatic` impl)
    let value = "qwer".to_string();
    let original: S3<'_, String> = S3{value: &value};
    let erased: ErasedRef<'_> = original.as_erased();
    erased.assert_is::<S3<'_, String>>();
    let restored = erased.restore::<S3<String>>().unwrap();
    assert_eq!(restored, &original);
}
#[cfg_attr(test, test)]
pub(super) fn test_owned() {
    let value = "qwer".to_string();
    let original: S<'_> = S{value: &value};
    let erased: Erased<'_> = original.into_erased();
    erased.assert_is::<S>();
    let restored = erased.restore::<S>().unwrap();
    assert_eq!(restored.value, &value);
}
#[cfg_attr(test, test)]
pub(super) fn test_mut() {
    let value = "qwer".to_string();
    let mut original: S<'_> = S{value: &value};
    let erased: ErasedMut = original.as_erased_mut();
    erased.assert_is::<S>();
    let restored = erased.restore::<S>().unwrap();
    assert_eq!(restored.value, &value);
}

#[cfg(feature = "derive")]
#[test]
pub fn test_derive() {
    use crate as transient_any;

    #[derive(MakeStatic)]
    struct S<'a, T> {
        value: &'a T,
    }
    let value = "qwer".to_string();
    let original: S<String> = S{value: &value};
    let erased: Erased = original.into_erased();
    erased.assert_is::<S<String>>();
    let restored = erased.restore::<S<String>>().unwrap();
    assert_eq!(restored.value, &value);
}
