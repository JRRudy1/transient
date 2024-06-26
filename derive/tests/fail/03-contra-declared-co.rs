use transient::Transient;

#[derive(Transient)]
#[covariant(a)]
struct ContravariantType<'a, T> {
    value: fn(&'a T),
}

fn main() {
    // this test should fail to compile
}
