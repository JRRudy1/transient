//! Ensures that a covariant usage of an invariant wrapper gets rejected

#![allow(dead_code)]
use transient::*;


/// This function requires the two arguments to have a matching lifetime.
/// If the given string lives longer than `long`, it should be allowed to
/// shortened due to *covariance*; however the opposite should be rejected
/// since `long` is *invariant* and cannot be shortened.
fn shrink<'a>(long: Box<dyn Any<Inv<'a>>>, _short: &'a String) -> Box<dyn Any<Inv<'a>>> {
    long
}

/// This test should fail to compile
fn main() {
    let static_: Box<dyn Any<()>> = Box::new(5_usize);
    let long: Box<dyn Any<Inv<'static>>> = static_.transcend();
    {
        let string = "short".to_string();
        // `long` is `'static` but `string` is `'short`, so
        // `Erased<'static, Invariant<'static>>` must be cast to
        // `Erased<'short, Invariant<'short>>` which should be
        // rejected for an *invariant* wrapper
        let shortened = shrink(long, &string);
        assert_eq!(shortened.static_type_id(), std::any::TypeId::of::<usize>())
    }
}
