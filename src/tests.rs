//! Tests for various parts of the Transient API

use crate::{tr::Transient, Any, Co, Contra, Inv, TypeId};

#[cfg(feature = "alloc")]
use alloc::{
    boxed::Box,
    string::{String, ToString},
};

/// Tests for a covariant struct with no generic type parameters.
#[cfg(feature = "alloc")]
mod covariant {
    use super::*;
    use crate::Downcast;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a> {
        value: &'a String,
    }
    unsafe impl<'a> Transient for S<'a> {
        type Static = S<'static>;
        type Transience = Co<'a>;
    }

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original: S<'_> = S { value: &value };
        let erased: Box<dyn Any<Co<'_>> + '_> = Box::new(original.clone());

        // `S::Transience` is `Co<'a>` se we can erase to `Any<Co<'a>>`, but
        // instead we downgraded to `Any<Inv<'a>>`. Now can't restore it b/c
        // the bounds require a subtype of `Co<'a>`, which `Inv<'a>` is not.
        // However, we need to allow the transition, but only when restoring,
        // not transcending.
        let restored: Box<S<'_>> = erased.downcast::<S<'_>>().unwrap();
        assert_eq!(*restored, original);
    }
    #[test]
    pub(super) fn test_ref() {
        let value = "qwer".to_string();
        let original = S { value: &value };
        let erased: &dyn Any<Co> = &original;
        assert_eq!(erased.type_id(), TypeId::of::<S>());
        let restored = erased.downcast_ref::<S>().unwrap();
        assert_eq!(restored, &original);
    }
    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = S { value: &value };
        let erased: &mut dyn Any<Co> = &mut original;
        assert_eq!(erased.type_id(), TypeId::of::<S>());
        let restored = erased.downcast_mut::<S>().unwrap().clone();
        assert_eq!(restored, original);
    }
}

/// Tests for a struct with a generic type parameter
#[cfg(any(feature = "std", feature = "alloc"))]
mod one_type_param {
    use super::*;
    use crate::Downcast;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a, T> {
        value: &'a T,
    }
    unsafe impl<'a, T: 'static> Transient for S<'a, T> {
        type Static = S<'static, T>;
        type Transience = Inv<'a>;
    }
    type SS<'a> = S<'a, String>;

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original = SS { value: &value };
        let erased: Box<dyn Any<Inv>> = Box::new(original.clone());
        assert_eq!(erased.type_id(), TypeId::of::<SS>());
        let restored = erased.downcast::<SS>().unwrap();
        assert_eq!(*restored, original);
    }

    #[test]
    pub(super) fn test_ref() {
        let value = "qwer".to_string();
        let original = SS { value: &value };
        let erased: &dyn Any<Inv> = &original;
        assert_eq!(erased.type_id(), TypeId::of::<SS>());
        let restored = erased.downcast_ref::<SS>().unwrap();
        assert_eq!(restored, &original);
    }

    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = SS { value: &value };
        let erased: &mut dyn Any<Inv> = &mut original;
        assert_eq!(erased.type_id(), TypeId::of::<SS>());
        let restored = erased.downcast_mut::<SS>().unwrap().clone();
        assert_eq!(restored, original);
    }
}

/// Tests for a struct with a two generic type parameters
mod two_type_params {
    use super::*;

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a, T1, T2> {
        value1: &'a T1,
        value2: &'a T2,
    }
    unsafe impl<'a, T1: 'static, T2: 'static> Transient for S<'a, T1, T2> {
        type Static = S<'static, T1, T2>;
        type Transience = Inv<'a>;
    }
}

/// Tests for a struct with two lifetime params that have different variances
mod mixed_lifetimes {
    use super::*;

    type ContraCo<'s, 'l> = (Contra<'s>, Co<'l>);

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct M<'s, 'l> {
        func: fn(&'s str) -> &'static str,
        string: &'l str,
    }
    unsafe impl<'s, 'l> Transient for M<'s, 'l> {
        type Static = M<'static, 'static>;
        type Transience = ContraCo<'s, 'l>;
    }
    fn requires_static(_value: &dyn Any<ContraCo<'static, '_>>) {
        /* ... */
    }

    /// This function requires the first lifetime to lengthen from `'short` to
    /// `'long` (contravariance), and the second lifetime parameter to shorten
    /// from `'long` to `'short` (covariance).
    fn lengthen<'b, 'short, 'long: 'short>(
        short: &'b M<'short, 'long>,
    ) -> &'b dyn Any<ContraCo<'long, 'short>> {
        short
    }

    const STATIC_STR: &str = "static";

    #[test]
    #[cfg(any(feature = "std", feature = "alloc"))]
    #[rustfmt::skip]
    fn test_owned() {
        let short: M<'_, 'static> = M { func: |_| "!", string: STATIC_STR };
        let long: M<'static, '_> = M { func: |s| s, string: STATIC_STR };
        let _ = lengthen(&short);
        
        let erased_short: Box<dyn Any<ContraCo> + '_> = Box::new(short);
        assert_eq!(erased_short.type_id(), TypeId::of::<M>());
        // the first (contra) param must lengthen from `'_` to `'static`
        requires_static(&*erased_short);

        let erased_long: Box<dyn Any<ContraCo> + '_> = Box::new(long);
        assert_eq!(erased_long.type_id(), TypeId::of::<M>());
    }

    #[test]
    #[rustfmt::skip]
    fn test_ref() {
        let short: M<'_, 'static> = M { func: |_| "!", string: STATIC_STR };
        let long: M<'static, '_> = M { func: |s| s, string: STATIC_STR };
        let _ = lengthen(&short);
        
        let erased_short: &dyn Any<ContraCo> = &short;
        assert_eq!(erased_short.type_id(), TypeId::of::<M>());
        
        // the first (contra) param must lengthen from `'_` to `'static`
        requires_static(erased_short);

        let erased_long: &dyn Any<ContraCo> = &long;
        assert_eq!(erased_long.type_id(), TypeId::of::<M>());
    }
}
