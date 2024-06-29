use transient::Transient;

#[derive(Transient)]
#[covariant(a)]
#[contravariant(a)]
struct CoReplacedByContra<'a, 'b, T> {
    value: fn(&'a T) -> &'b T
}

#[derive(Transient)]
#[covariant]
#[contravariant(a)]
struct GlobalCoReplacedByContra<'a, 'b, T> {
    value: fn(&'a T) -> &'b T
}

#[derive(Transient)]
#[covariant(a, a)]
struct CoDeclaredTwice<'a, 'b, T> {
    value1: &'a T,
    value2: &'b T,
}

fn main() {
    // this test should fail to compile
}
