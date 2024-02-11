#![allow(dead_code)]
use transient::*;
use transient::{Transient, Contravariant};

struct S<'a> {
    func: fn(&'a str),
}

fn use_temp_t<'a>(_value: &'a str) {
    /* ... */
}

unsafe impl<'a> Transient for S<'a> {
    type Static = S<'static>;
    type Transience = Contravariant<'a>;
}

// This function requires `short` to lengthen its lifetime from 'a to 'b, which
// is only allowed if the compiler recognizes it as *contravariant*.
fn lengthen<'a, 'b: 'a>(short: ErasedContra<'a>, _long: &'b str) -> ErasedContra<'b> {
    short
}

fn main() {

    let static_str: &'static str = "static";

    let short_obj: S<'_> = S{func: use_temp_t};
    let short: ErasedContra<'_> = short_obj.erase();

    {
        // `short` is `'short` but `static_str` is `'static`, so the cast from
        // `ErasedContra<'short>` to `ErasedContra<'static>` requires *contravariance*
        let lengthened: ErasedContra<'static> = lengthen(short, static_str);
        assert_eq!(lengthened.type_id(), std::any::TypeId::of::<S<'static>>())
    }
}
