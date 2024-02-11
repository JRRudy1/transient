use transient::*;

// This function requires the *contravariant* first lifetime parameter
// to shorten from 'long to 'short, which should be *rejected* by the
// borrow checker.
fn shorten<'b, 'short, 'long: 'short>(
    long_long: ErasedRef<'b, (Contravariant<'long>, Covariant<'long>)>,
    _long: &'long str
) -> ErasedRef<'b, (Contravariant<'short>, Covariant<'long>)> {
    long_long
}

fn main() {
    // this test should fail to compile
}
