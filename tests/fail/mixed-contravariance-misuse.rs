use transient::*;

struct S<'a, 'b>(fn(&'a usize) -> &'b str);

unsafe impl<'a, 'b> Transient for S<'a, 'b> {
    type Static = S<'static, 'static>;
    type Transience = (Contra<'a>, Co<'b>);
}

// This function requires the *contravariant* first lifetime parameter
// to shorten from 'long to 'short, which should be *rejected* by the
// borrow checker.
fn shorten<'b, 'short, 'long: 'short>(
    long_long: &'b S<'long, 'short>,
) -> &'b dyn Any<(Contra<'short>, Co<'short>)> {
    long_long
}

fn main() {
    // this test should fail to compile
}
