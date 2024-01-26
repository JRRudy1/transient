
/// Tests for a simple struct with no generic parameters.
mod basic {
    use crate::{MakeStatic, TransientAny};
    
    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a> {
        value: &'a String
    }
    unsafe impl<'a> MakeStatic<'a> for S<'a> {
        type Static = S<'static>;
    }

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original = S{value: &value};
        let erased = original.clone().into_erased();
        assert_eq!(erased.type_id(), S::type_id());
        let restored = erased.restore::<S>().unwrap();
        assert_eq!(restored, original);
    }
    #[test]
    pub(super) fn test_ref() { // single lifetime (derived `MakeStatic` impl)
        let value = "qwer".to_string();
        let original = S{value: &value};
        let erased = original.as_erased();
        assert_eq!(erased.type_id(), S::type_id());
        let restored = erased.restore::<S>().unwrap();
        assert_eq!(restored, &original);
    }
    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = S{value: &value};
        let erased = original.as_erased_mut();
        assert_eq!(erased.type_id(), S::type_id());
        let restored = erased.restore::<S>().unwrap().clone();
        assert_eq!(restored, original);
    }
}


/// Tests for a struct with generic parameters.
mod generics {
    use crate::{MakeStatic, TransientAny};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a, T: 'static> {
        value: &'a T,
    }
    unsafe impl<'a, T: 'static> MakeStatic<'a> for S<'a, T> {
        type Static = S<'static, T>;
    }

    type SS<'a> = S<'a, String>;

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original = SS{value: &value};
        let erased = original.clone().into_erased();
        assert_eq!(erased.type_id(), SS::type_id());
        let restored = erased.restore::<SS>().unwrap();
        assert_eq!(restored, original);
    }

    #[test]
    pub(super) fn test_ref() {
        let value = "qwer".to_string();
        let original = SS{value: &value};
        let erased = original.as_erased();
        assert_eq!(erased.type_id(), SS::type_id());
        let restored = erased.restore::<SS>().unwrap();
        assert_eq!(restored, &original);
    }

    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = SS{value: &value};
        let erased = original.as_erased_mut();
        assert_eq!(erased.type_id(), SS::type_id());
        let restored = erased.restore::<SS>().unwrap().clone();
        assert_eq!(restored, original);
    }
}


/// Tests for a struct with more than one lifetime parameter.
mod multi_lifetime {
    use crate::{MakeStatic, TransientAny};


    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a, 'b: 'a> {
        short: &'a String,
        long: &'b String,
    }
    unsafe impl<'a, 'b: 'a> MakeStatic<'a> for S<'a, 'b> {
        type Static = S<'static, 'static>;
    }

    #[test]
    pub(super) fn test_ref() {
        let (short, long) = ("short".to_string(), "long".to_string());
        let original = S {short: &short, long: &long};
        let erased = original.as_erased();
        assert_eq!(erased.type_id(), S::type_id());
        let restored = erased.restore::<S>().unwrap();
        assert_eq!(restored, &original);
    }
}


/// Tests for impls generated by the derive macro
#[cfg(feature = "derive")]
mod derived {
    use crate as transient_any;  // workaround for macro hygiene
    use crate::TransientAny;


    #[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
    struct S<'a, T> {
        value: &'a T,
    }
    type SS<'a> = S<'a, String>;

    #[test]
    pub fn test_transient() {
        let string = "qwer".to_string();
        let original = SS{value: &string};
        let erased = original.clone().into_erased();
        assert_eq!(erased.type_id(), SS::type_id());
        let restored = erased.restore::<SS>().unwrap();
        assert_eq!(restored, original);
    }

    #[derive(TransientAny, Clone, Debug, PartialEq, Eq)]
    struct S2<T> {
        value: T,
    }
    type SS2 = S2<String>;

    #[test]
    pub fn test_static() {
        let original = SS2{value: "qwer".to_string()};
        let erased = original.clone().into_erased();
        assert_eq!(erased.type_id(), SS2::type_id());
        let restored = erased.restore::<SS2>().unwrap();
        assert_eq!(restored, original);
    }
}
