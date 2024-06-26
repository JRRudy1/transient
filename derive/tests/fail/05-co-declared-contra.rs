use transient::Transient;

#[derive(Transient)]
#[contravariant(a)]
struct CovariantType<'a, T> {
    value: &'a T,
}

fn main() {
    // this test should fail to compile
}
