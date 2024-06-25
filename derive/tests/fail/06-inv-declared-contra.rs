use transient::Transient;

#[derive(Transient)]
#[contravariant(a)]
struct InvariantType<'a, T> {
    value: fn(&'a T) -> &'a T,
}

fn main() {
    // this test should fail to compile
}
