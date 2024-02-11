
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
    use crate::{Invariant, Transient};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a> {
        value: &'a String
    }
    unsafe impl<'a> Transient for S<'a> {
        type Static = S<'static>;
        type Transience = Invariant<'a>;
    }

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original = S{value: &value};
        let erased = original.clone().ierase();
        assert_eq!(erased.type_id(), S::static_type_id());
        let restored = erased.restore::<S>().unwrap();
        assert_eq!(restored, original);
    }
    #[test]
    pub(super) fn test_ref() { // single lifetime (derived `Transient` impl)
        let value = "qwer".to_string();
        let original = S{value: &value};
        let erased = original.erase_ref();
        assert_eq!(erased.type_id(), S::static_type_id());
        let restored = erased.restore::<S>().unwrap();
        assert_eq!(restored, &original);
    }
    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = S{value: &value};
        let erased = original.erase_mut();
        assert_eq!(erased.type_id(), S::static_type_id());
        let restored = erased.restore::<S>().unwrap().clone();
        assert_eq!(restored, original);
    }
}


/// Tests for a struct with generic parameters.
mod generics {
    use crate::{Invariant, Transient};

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
        let erased = original.clone().ierase();
        assert_eq!(erased.type_id(), SS::static_type_id());
        let restored = erased.restore::<SS>().unwrap();
        assert_eq!(restored, original);
    }

    #[test]
    pub(super) fn test_ref() {
        let value = "qwer".to_string();
        let original = SS{value: &value};
        let erased = original.erase_ref();
        assert_eq!(erased.type_id(), SS::static_type_id());
        let restored = erased.restore::<SS>().unwrap();
        assert_eq!(restored, &original);
    }

    #[test]
    pub(super) fn test_mut() {
        let value = "qwer".to_string();
        let mut original = SS{value: &value};
        let erased = original.erase_mut();
        assert_eq!(erased.type_id(), SS::static_type_id());
        let restored = erased.restore::<SS>().unwrap().clone();
        assert_eq!(restored, original);
    }
}


/// Tests for a struct with more than one lifetime parameter.
#[allow(unused)]
mod multi_lifetime {
    use std::any::Any;
    use crate::{transient::Transient, Erased, Transience, Invariant, Covariant};

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


/*
/// Tests for a struct with generic parameters.
#[allow(dead_code, unused)]
mod contravariance {
    use crate::{Transient, Erased, ErasedRef, variance};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a> {
        // contravariant over `&'a T`; `f(&'static T)` not ok
        func: fn(&'a str),
    }


    unsafe impl<'a> Transient<'a> for S<'a> {
        type Static = S<'static>;
        type Variance = variance::Invariant<'a>;
    }

    /// If `s: S<'static>` and `arg: &'short`, `s.func` could not
    /// be called with `arg`!
    fn use_s<'a>(s: &S<'a>, arg: &'a str) {
        (s.func)(arg);
    }

    // function that requires a static reference
    fn _requires_static(x: &'static str) {
        /* ... */
    }
    // function that only requires a temporary reference
    fn _allows_transient(x: &str) {
        /* ... */
    }

    fn use_erased<'short>(erased: &'_ Erased<'short, variance::Contravariant>, arg: &'short str) {
        //
        // # Can i do something bad by passing `&'b Erased<'static>` instead?

        // Assume that `Erased<'src>` is defined to be covariant with `'src`.
        //
        // If `Erased<'static>` is passed to a function expecting `Erased<'short>`

        // - the wrapped func is `fn(&'static str)` and might use the `str` forever
        // - the original type wrapping the func is `S<'static>`
        // - we erased it to `Erased<'static>`
        // - compiler shortens it to `Erased<'short>` when this func is called
        // - we call `restore` to downcast as `S<'short>`
        // - this implies that the wrapped func is `fn(&'short str)` (*** FALSE ***)
        // - the compiler then lets us call `s.func` with `arg: &'short str`

        // - the true function `fn(&'static str)` expects permanent access to `arg`
        // - BUT the compiler thinks it only needs it for `'short`

        // - eventually `'short` ends and the referenced `String` is allowed to be dropped
        // - but `s.func` might have put a `'static` reference to it somewhere for later use
        let x: ErasedRef<'_, 'short> = erased.as_ref();
        // F.func requires a
        let r: &'_ S<'short> = x.restore().unwrap();
        todo!()
    }
/*
    #[test]
    pub(super) fn test_owned() {

        let string = format!("{:?}", 5);

        let transient_str: &'_ str = &string;
        let static_str: &'static str = "literal";

        let allows_transient: S<'_> = S{func: _allows_transient};
        let requires_static: S<'static> = S{func: _requires_static};

        use_s(&allows_transient, transient_str); // OK
        use_s(&requires_static, static_str); // OK
        use_s(&allows_transient, static_str); // OK

        //// The next line would require `'a: 'static` and fail:
        // use_s(&requires_static, transient_str); // NOT OK!

        //// Should be OK:
        // `S<'a>` => `S<'static>` => `dyn Any + 'static>` => `Erased<'a>`
        let erased: Erased<'_, _> = allows_transient.clone().erase();
        // `Erased<'a>` => `dyn Any + 'static>` => `S<'static>` => `S<'a>`
        let restored: S<'_> = erased.restore().unwrap();

        //// ?
        // `S<'static>` => `S<'static>` => `dyn Any + 'static>` => `Erased<'static>`
        let erased: Erased<'static, _> = requires_static.erase();

        let x = use_erased(&erased, transient_str);


        // `Erased<'static>` => `dyn Any + 'static>` => `S<'static>` => `S<'static>`
        let restored: S<'_> = erased.restore().unwrap();


        assert_eq!(1, 1);
    }*/
}
*/
/*
/// Tests for impls generated by the derive macro
#[cfg(feature = "derive")]
mod derived {
    use crate as transient_any;  // workaround for macro hygiene
    use crate::Transient;

    #[derive(Transient, Clone, Debug, PartialEq, Eq)]
    struct S<'a, T> {
        value: &'a T,
    }
    type SS<'a> = S<'a, String>;

    #[test]
    pub fn test_transient() {
        let string = "qwer".to_string();
        let original = SS{value: &string};
        let erased = original.clone().erase();
        assert_eq!(erased.type_id(), SS::static_type_id());
        let restored = erased.restore::<SS>().unwrap();
        assert_eq!(restored, original);
    }

    #[derive(Transient, Clone, Debug, PartialEq, Eq)]
    struct S2<T> {
        value: T,
    }
    type SS2 = S2<String>;

    #[test]
    pub fn test_static() {
        let original = SS2{value: "qwer".to_string()};
        let erased = original.clone().erase();
        assert_eq!(erased.type_id(), SS2::static_type_id());
        let restored = erased.restore::<SS2>().unwrap();
        assert_eq!(restored, original);
    }
}

*/

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
    fn requires_static(_value: ErasedRef<ContraCo<'static, '_>>) {
        /* ... */
    }

    /// This function requires the first lifetime to lengthen from `'short` to
    /// `'long` (contravariance), and the second lifetime parameter to shorten
    /// from `'long` to `'short` (covariance).
    fn lengthen<'b, 'short, 'long: 'short>(
        short: ErasedRef<'b, ContraCo<'short, 'long>>,
    ) -> ErasedRef<'b, ContraCo<'long, 'short>> {
        short
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
        let erased_short = short.erase_ref();
        assert_eq!(erased_short.type_id(), M::static_type_id());
        // the first (contra) param must lengthen from `'_` to `'static`
        requires_static(erased_short);

        let erased_long = long.erase_ref();
        assert_eq!(erased_long.type_id(), M::static_type_id());
    }
}