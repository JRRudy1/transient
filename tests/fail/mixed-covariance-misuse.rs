use transient::*;

// This function requires the *covariant* second lifetime parameter
// to lengthen from 'short to 'long, which should be *rejected* by
// the borrow checker.
fn lengthen<'b, 'short, 'long: 'short>(
    short_short: ErasedRef<'b, (Contravariant<'short>, Covariant<'short>)>,
    _long: &'long str
) -> ErasedRef<'b, (Contravariant<'short>, Covariant<'long>)> {
    short_short
}

fn main() {
    // this test should fail to compile
}
