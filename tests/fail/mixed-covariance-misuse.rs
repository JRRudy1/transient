use transient::*;

struct S<'a, 'b>(fn(&'a usize) -> &'b str);

unsafe impl<'a, 'b> Transient for S<'a, 'b> {
    type Static = S<'static, 'static>;
    type Transience = (Contra<'a>, Co<'b>);
}

// This function requires the *covariant* second lifetime parameter
// to lengthen from 'short to 'long, which should be *rejected* by
// the borrow checker.
fn lengthen<'b, 'short, 'long: 'short>(
    short_short: &'b S<'short, 'short>,
) -> &'b dyn Any<(Contra<'short>, Co<'long>)> {
    short_short
}

fn main() {
    // this test should fail to compile
}
