
use transient::Transient;

#[derive(Transient)]
#[variance('b = co)]
struct SsWithLifetime<'a> {
    val: &'a str,
}

#[derive(Transient)]
#[variance('b = co)]
struct SsNoLifetime {
    val: &'static str,
}

fn main() {}
