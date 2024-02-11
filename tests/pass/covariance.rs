
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
fn shrink<'a, 'b: 'a>(long: Box<dyn Any<Co<'b>>>) -> Box<dyn Any<Co<'a>>> {
    long.transcend()
}

fn main() {
    // by default a `usize` will `v_erase` to `Static` variance, but use `Into` to convert
    let static_: Box<dyn Any> = 5_usize.erase();
    let long: Box<dyn Any<Co>> = static_.transcend();
    {
        let string = "short".to_string();
        // `long` is `'static` but `string` is `'short`, so the cast from
        // `Erased<'static, Covariant<'static>>` to `Erased<'short, Covariant<'short>>`
        // requires *covariance*
        let shortened = shrink(long);
        assert_eq!(shortened.type_id(), std::any::TypeId::of::<usize>())
    }
}
