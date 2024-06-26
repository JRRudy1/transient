
use transient::Transient;

#[derive(Transient)]
#[variance('a = contra)]
struct SsActualCoVariant<'a> {
    val: &'a str,
}

#[derive(Transient)]
#[variance('a = contra)]
struct SsActualInvariant<'a> {
    val_co: &'a str,
    val_contra: fn(&'a str),
}

fn main() {}
