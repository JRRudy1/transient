use transient::*;

// This function requires the *covariant* second lifetime parameter
// to lengthen from 'short to 'long, which should be *rejected* by
// the borrow checker.
fn lengthen<'b, 'short, 'long: 'short>(
    short_short: &'b dyn Any<(Contra<'short>, Co<'short>)>,
) -> &'b dyn Any<(Contra<'short>, Co<'long>)> {
    short_short.transcend_ref()
}

fn main() {
    // this test should fail to compile
}
