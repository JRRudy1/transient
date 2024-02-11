//! Ensures that a covariant usage of an invariant wrapper gets rejected

#![allow(dead_code)]
use transient::*;


/// This function requires the two arguments to have a matching lifetime.
/// If the given string lives longer than `long`, it should be allowed to
/// shortened due to *covariance*; however the opposite should be rejected
/// since `long` is *invariant* and cannot be shortened.
fn shrink<'a>(long: ErasedInv<'a>, _short: &'a String) -> ErasedInv<'a> {
    long
}

/// This test should fail to compile
fn main() {
    let static_: Erased<Static> = 5_usize.erase();
    let long: ErasedInv<'static> = static_.into_transience();
    {
        let string = "short".to_string();
        // `long` is `'static` but `string` is `'short`, so
        // `Erased<'static, Invariant<'static>>` must be cast to
        // `Erased<'short, Invariant<'short>>` which should be
        // rejected for an *invariant* wrapper
        let shortened = shrink(long, &string);
        assert_eq!(shortened.type_id(), std::any::TypeId::of::<usize>())
    }
}
