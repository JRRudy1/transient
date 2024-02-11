
use transient::*;

struct TypeAndLifetime<'a, T> {
    value: &'a T,
}
unsafe impl<'a, T: 'static> Transient for TypeAndLifetime<'a, T> {
    type Static = TypeAndLifetime<'static, T>;
    type Transience = Covariant<'a>;
}

// This function requires `long` to shorten its lifetime from 'b to 'a, which
// is only allowed if the compiler recognizes it as *covariant*.
fn shrink<'a, 'b: 'a>(long: ErasedCo<'b>, _short: &'a String) -> ErasedCo<'a> {
    long
}

fn main() {
    // by default a `usize` will `v_erase` to `Static` variance, but use `Into` to convert
    let static_: Erased<Static> = 5_usize.erase();
    let long: ErasedCo<'static> = static_.into_transience();
    {
        let string = "short".to_string();
        // `long` is `'static` but `string` is `'short`, so the cast from
        // `Erased<'static, Covariant<'static>>` to `Erased<'short, Covariant<'short>>`
        // requires *covariance*
        let shortened = shrink(long, &string);
        assert_eq!(shortened.type_id(), std::any::TypeId::of::<usize>())
    }
}
