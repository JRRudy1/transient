
/// Tests for a simple struct with no generic parameters.
mod basic {
    use crate::{Invariant, TransientAny};
    
    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a> {
        value: &'a String
    }
    unsafe impl<'a> TransientAny<'a> for S<'a> {
        type Static = S<'static>;
        type Variance = Invariant<'a>;
    }

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original = S{value: &value};
        let erased = original.clone().erase();
        assert_eq!(erased.type_id(), S::static_type_id());
        let restored = erased.restore::<S>().unwrap();
        assert_eq!(restored, original);
    }
    #[test]
    pub(super) fn test_ref() { // single lifetime (derived `TransientAny` impl)
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
    use crate::{Invariant, TransientAny};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a, T> {
        value: &'a T,
    }
    unsafe impl<'a, T: 'static> TransientAny<'a> for S<'a, T> {
        type Static = S<'static, T>;
        type Variance = Invariant<'a>;
    }

    type SS<'a> = S<'a, String>;

    #[test]
    pub(super) fn test_owned() {
        let value = "qwer".to_string();
        let original = SS{value: &value};
        let erased = original.clone().erase();
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
    use std::marker::PhantomData;
    use crate::{TransientAny, ErasedRef, Erased, Variance, VarianceTag};

    pub type Invariant<'a, 'b> = PhantomData<fn(&'a &'b ()) -> &'a &'b ()>;

    unsafe impl<'a, 'b> Variance for Invariant<'a, 'b> {
        const TAG: VarianceTag = VarianceTag::Invariant;
    }

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'s, 'l: 's> {
        short: &'s str,
        long: &'l str,
    }
    unsafe impl<'s, 'l: 's> TransientAny<'s> for S<'s, 'l> {
        type Static = S<'static, 'static>;
        type Variance = Invariant<'s, 'l>;
    }

    fn f0<'b, 's, 'l: 's, V: Variance>(short: &'b Erased<'s, V>, long: &'l str) -> &'l str {
        long
    }
    fn f<'s, 'l: 's>(short: &mut &'s str, long: &mut &'l str) -> &'static str {

        let original: S<'s, 'l> = S {short: *short, long: *long}; // impl `TransientAny<'s>`
        let erased: Erased<'s, Invariant<'s, 'l>> = Erased::new(original);

        let _: &'l str = f0(&erased, long);
        // `S<'s, 'static>: TransientAny<'s>`
        let restored: S<'s, 'static> = erased.restore().unwrap();

        let short2: &'s str = restored.short;
        // `long2` isn't actually static! It only lives for `'l`
        let long2: &'static str = restored.long;

        // assert_eq!(*long, restored.long);
        long2
    }

    #[test]
    pub(super) fn test_ref() {
        let fake_static: &'static str;
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
    }
}


/*
/// Tests for a struct with generic parameters.
#[allow(dead_code, unused)]
mod contravariance {
    use crate::{TransientAny, Erased, ErasedRef, variance};

    #[derive(Debug, Clone, PartialEq, Eq)]
    struct S<'a> {
        // contravariant over `&'a T`; `f(&'static T)` not ok
        func: fn(&'a str),
    }


    unsafe impl<'a> TransientAny<'a> for S<'a> {
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
        let erased = original.clone().erase();
        assert_eq!(erased.type_id(), SS::static_type_id());
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
        let erased = original.clone().erase();
        assert_eq!(erased.type_id(), SS2::static_type_id());
        let restored = erased.restore::<SS2>().unwrap();
        assert_eq!(restored, original);
    }
}


#[allow(unused, dead_code)]
mod variance {
    use std::marker::PhantomData;
    use std::cell::UnsafeCell;

    struct CovariantStruct<'a> {
        value: &'a String  // struct is covariant over 'a
    }
    struct InvariantStruct<'a> {
        value: &'a mut String  // struct is invariant over 'a
    }

    struct Wrapper<'a, V> (
        &'a String,  // struct is invariant over 'a
        PhantomData<V>,
        // PhantomData<&'a ()>
    );

    impl<'short> InvWrapper<'short> {

        fn test_co<'long: 'short>(s: &'short String, t: &'long String) {
            let long: CoWrapper<'long> = Wrapper(t, PhantomData);
            let shortened: CoWrapper<'short> = long;
        }

        fn test_inv<'long: 'short>(s: &'short String, t: &'long String) {
            // let long: InvWrapper<'long> = Wrapper(t, PhantomData);
            // let shortened: InvWrapper<'short> = long;
        }

    }


    type InvWrapper0<'a> = Wrapper<'a, &'a mut &'a mut ()>;
    type InvWrapper<'a> = Wrapper<'a, fn(&'a ()) -> &'a ()>;
    type CoWrapper<'a> = Wrapper<'a, &'a ()>;

    // 'long must shorten to 'short
    fn f<T>(short: &T, long: &T) {
        ()
    }
    fn nf<'a>(arg: &'a ()) -> &'a () {
        arg
    }

    fn test_func<'short, 'long: 'short, V>(s: &'short String, t: &'long String) {

        let short: Wrapper<'short, V> = Wrapper(s, PhantomData);
        let long: Wrapper<'long, V> = Wrapper(t, PhantomData);

        let shortened: Wrapper<'short, V> = long;
    }

    fn shrink<'short, 'long: 'short, V>(long: Wrapper<'long, V>, _: &'short String) -> Wrapper<'short, V> {
        long
    }

    fn cshrink<'short, 'long: 'short>(long: CoWrapper<'long>, s: &'short String) -> CoWrapper<'short> {
        let x: CoWrapper<'short> = shrink::<'short, 'long, _>(long, s);
        // shrink(long, s)
        todo!()
    }

    // fn ishrink<'short, 'long: 'short>(long: InvWrapper<'long>, s: &'short String) -> InvWrapper<'short> {
    //     let x: InvWrapper<'_> = shrink::<'short, 'long, _>(long, s);
    //
    //     todo!()
    // }


    #[test]
    fn test_covariant() {
        let mut long_string: String = "long".to_string();
        let long: InvWrapper = Wrapper(&long_string, PhantomData::<fn(&'_ ()) -> &'_ ()>);

        {
        let short_string: String = "short".to_string();
        let short: InvWrapper = Wrapper(&short_string, PhantomData::<fn(&'_ ()) -> &'_ ()>);

        test_func::<fn(&'_ ()) -> &'_ ()>(&short_string, &long_string);

        let shortened = shrink(long, &short_string);

        let _ = shortened;

        // f(&short, &long)
        }
        assert_eq!(&long_string, "long");
    }
    #[test]
    fn test_invariant() {
        let mut long_string: String = "long".to_string();
        let long = InvariantStruct{value: &mut long_string};
        {
        let mut short_string: String = "short".to_string();
        let short = InvariantStruct{value: &mut short_string};
        f(&short, &long)
        }
        assert_eq!(&long_string, "long");
    }


}

#[test]
fn x() {
    let t = trybuild::TestCases::new();
    t.pass("tests/variance/covariance.rs");
    t.compile_fail("tests/variance/invariance.rs");
    t.compile_fail("tests/variance/invariance2.rs");
}

