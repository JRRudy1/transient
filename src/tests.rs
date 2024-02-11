
/// Tests for a simple struct with no generic parameters.
mod double {
    use crate::{Invariant, Transient};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a, T1, T2> {
        value1: &'a T1,
        value2: &'a T2,

    }
    unsafe impl<'a, T1: 'static, T2: 'static> Transient for S<'a, T1, T2> {
        type Static = S<'static, T1, T2>;
        type Transience = Invariant<'a>;
    }

}

/// Tests for a simple struct with no generic parameters.
mod basic {
    use crate::{Invariant, Transient, Any, AnyOps, Co, Inv, Contra};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a> {
        value: &'a String
    }
    unsafe impl<'a> Transient for S<'a> {
        type Static = S<'static>;
        type Transience = Co<'a>;
    }

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original: S<'_> = S{value: &value};
        let erased: Box<dyn Any<Co<'_>> + '_> = Box::new(original.clone());
        // assert_eq!(erased.type_id(), S::static_type_id());

        // `S::Transience` is `Co<'a>` se we can erase to `Any<Co<'a>>`, but
        // instead we downgraded to `Any<Inv<'a>>`. Now can't restore it b/c
        // the bounds require a subtype of `Co<'a>`, which `Inv<'a>` is not.
        // However, we need to allow the transition, but only when restoring,
        // not transcending.
        let restored: Box<S<'_>> = erased.downcast::<S<'_>>().unwrap();
        // assert_eq!(*restored, original);
    }
    #[test]
    pub(super) fn test_ref() { // single lifetime (derived `Transient` impl)
        let value = "qwer".to_string();
        let original = S{value: &value};
        let erased: &dyn Any<Co> = &original;
        assert_eq!(erased.type_id(), S::static_type_id());
        let restored = erased.downcast_ref::<S>().unwrap();
        assert_eq!(restored, &original);
    }
    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = S{value: &value};
        let erased: &mut dyn Any<Co> = &mut original;
        assert_eq!(erased.type_id(), S::static_type_id());
        let restored = erased.downcast_mut::<S>().unwrap().clone();
        assert_eq!(restored, original);
    }
}


/// Tests for a struct with generic parameters.
mod generics {
    use crate::{Invariant, Transient, Any, AnyOps, Inv};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a, T> {
        value: &'a T,
    }
    unsafe impl<'a, T: 'static> Transient for S<'a, T> {
        type Static = S<'static, T>;
        type Transience = Invariant<'a>;
    }
    type SS<'a> = S<'a, String>;

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original = SS{value: &value};
        let erased: Box<dyn Any<Inv>> = Box::new(original.clone());
        assert_eq!(erased.type_id(), SS::static_type_id());
        let restored = erased.downcast::<SS>().unwrap();
        assert_eq!(*restored, original);
    }

    #[test]
    pub(super) fn test_ref() {
        let value = "qwer".to_string();
        let original = SS{value: &value};
        let erased: &dyn Any<_> = &original;
        assert_eq!(erased.type_id(), SS::static_type_id());
        let restored = erased.downcast_ref::<SS>().unwrap();
        assert_eq!(restored, &original);
    }

    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = SS{value: &value};
        let erased: &mut dyn Any<_> = &mut original;
        assert_eq!(erased.type_id(), SS::static_type_id());
        let restored = erased.downcast_mut::<SS>().unwrap().clone();
        assert_eq!(restored, original);
    }
}

/*
/// Tests for a struct with more than one lifetime parameter.
#[allow(unused)]
mod multi_lifetime {
    use std::any::Any;
    use crate::{transient::Transient, Transience, Invariant, Covariant};

    pub type InvInv<'a, 'b> = (Invariant<'a>, Invariant<'b>);
    pub type CoCo<'a, 'b> = (Covariant<'a>, Covariant<'b>);

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'s, 'l: 's> {
        short: &'s str,
        long: &'l str,
    }
    unsafe impl<'s, 'l: 's> Transient for S<'s, 'l> {
        type Static = S<'static, 'static>;
        type Transience = InvInv<'s, 'l>;
    }
    impl<'s, 'l: 's> S<'s, 'l> {

        fn restore_(erased: Box<dyn Any>, tr: InvInv<'s, 'l>) -> Result<Self, Box<dyn Any>>{
            let x: Box<S<'static, 'static>> = erased.downcast()?;
            let y: S<'static, 'static> = *x;
            let z: S<'s, 'l> = unsafe { std::mem::transmute(y) };
            Ok(z)
        }
    }

    fn f0<'b, 's, 'l: 's, V: Transience>(short: &'b Erased<V>, long: &'l str) -> &'l str {
        long
    }
    fn f<'s, 'l: 's>(short: &mut &'s str, long: &mut &'l str) -> &'l str {

        let original: S<'s, 'l> = S {short: *short, long: *long}; // impl `Transient<'s>`
        let erased: Erased<InvInv<'s, 'l>> = Erased::new(original);

        let _: &'l str = f0(&erased, long);
        // `S<'s, 'static>: Transient<'s>`
        let restored: S<'s, 'l> = erased.restore().unwrap();

        let short2: &'s str = restored.short;
        // `long2` isn't actually static! It only lives for `'l`
        let long2: &'l str = restored.long;

        // assert_eq!(*long, restored.long);
        long2
    }
    /*
    #[test]
    pub(super) fn test_ref() {
        let fake_static: &'_ str;
        {
            let mut long = "long".to_string();
            long.push_str("qqq");
            let mut long_ref: &str = &long;
            {
                let mut short = "short".to_string();
                short.push_str("qqq");
                let mut short_ref: &str = &short;

                fake_static = f(&mut short_ref, &mut long_ref);
                let _ = short;
                drop(short);
            }
            assert_eq!(long_ref, "long");
            drop(long);
        }

        let x = Box::new(5);
        let s = "string".to_string();

        assert_eq!(fake_static, "long");
        assert_eq!(&s, "string");

        let (short, long) = ("short".to_string(), "long".to_string());
        let original = S {short: &short, long: &long};
        let erased = original.erase_ref();
        assert_eq!(erased.type_id(), S::static_type_id());
        let restored = erased.restore::<S>().unwrap();
        assert_eq!(restored, &original);
    }*/
}
*/ // todo!

#[test]
fn variance_tests() {
    let t = trybuild::TestCases::new();
    t.pass("tests/pass/*.rs");
    t.compile_fail("tests/fail/*.rs");
}

#[allow(unused, dead_code)]
mod mixed_lifetimes {
    use crate::*;

    type ContraCo<'s, 'l> = (Contravariant<'s>, Covariant<'l>);


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
        short: &'b dyn Any<ContraCo<'short, 'long>>,
    ) -> &'b dyn Any<ContraCo<'long, 'short>> {
        short.transcend_ref()
    }

    #[test]
    fn test1() {
        let static_str = "static";

        let short: M<'_, 'static> = M {
            func: |_| "!",
            string: static_str,
        };
        let long: M<'static, '_> = M {
            func: |s| s,
            string: static_str,
        };
        let erased_short: Box<dyn Any<ContraCo>> = Box::new(short);
        // let erased_short = short.erase_ref();
        assert_eq!(erased_short.type_id(), M::static_type_id());
        // the first (contra) param must lengthen from `'_` to `'static`
        requires_static(&*erased_short);

        let erased_long: &dyn Any<ContraCo> = &long;
        assert_eq!(erased_long.type_id(), M::static_type_id());
    }
}