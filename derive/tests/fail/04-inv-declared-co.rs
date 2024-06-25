use transient::Transient;

#[derive(Transient)]
#[covariant(a)]
struct InvariantType<'a, T> {
    value: fn(&'a T) -> &'a T,
}

fn main() {
    // this test should fail to compile
}
