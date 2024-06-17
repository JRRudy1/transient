//! Ensures that a function signature requiring covariant use of an invariant
//! wrapper gets rejected
use transient::*;

// This function requires `long` to shorten its lifetime from 'b to 'a, and
// should be *rejected* if the type is considered to be *invariant*.
fn shrink<'a, 'b: 'a>(long: Box<dyn Any<Inv<'b>>>) -> Box<dyn Any<Inv<'a>>> {
    long.transcend()
}

fn main() {
    // This test should fail to compile
}
