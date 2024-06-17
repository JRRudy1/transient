//! Ensures that a function signature requiring covariant use of an invariant
//! wrapper gets rejected
use transient::*;

struct S<'a>(fn(&'a ()) -> &'a ());

unsafe impl<'a> Transient for S<'a> {
    type Static = S<'static>;
    type Transience = Inv<'a>;
}

// This function requires `long` to shorten its lifetime from 'b to 'a, and
// should be *rejected* if the type is considered to be *invariant*.
fn shrink<'a, 'b: 'a>(long: Box<S<'b>>) -> Box<dyn Any<Inv<'a>> + '_> {
    long
}

fn main() {
    // This test should fail to compile
}
