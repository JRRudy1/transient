//! Ensures that a function signature requiring covariant use of an invariant
//! wrapper gets rejected

#![allow(dead_code)]
use transient_any::*;


// This function requires `long` to shorten its lifetime from 'b to 'a, and
// should be *rejected* if the type is considered to be *invariant*.
fn shrink<'a, 'b: 'a>(long: ErasedInv<'b>, _short: &'a String) -> ErasedInv<'a> {
    long
}

fn main() {
    // This test should fail to compile
}