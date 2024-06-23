
use transient::Transient;

#[derive(Transient)]
#[variance('a = co)]
struct SsActualContraVariant<'a> {
    val: fn(&'a str),
}

#[derive(Transient)]
#[variance('a = co)]
struct SsActualInvariant<'a> {
    val_co: &'a str,
    val_contra: fn(&'a str),
}

fn main() {}
