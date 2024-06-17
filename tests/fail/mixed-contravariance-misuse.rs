use transient::*;

// This function requires the *contravariant* first lifetime parameter
// to shorten from 'long to 'short, which should be *rejected* by the
// borrow checker.
fn shorten<'b, 'short, 'long: 'short>(
    long_long: &'b dyn Any<(Contra<'long>, Co<'long>)>,
) -> &'b dyn Any<(Contra<'short>, Co<'long>)> {
    long_long.transcend_ref()
}

fn main() {
    // this test should fail to compile
}
