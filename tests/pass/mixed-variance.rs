use transient::*;

type ContraCo<'s, 'l> = (Contravariant<'s>, Covariant<'l>);

#[derive(Debug, Clone, PartialEq, Eq)]
struct M<'s, 'l> {
    func: fn(&'s str) -> &'static str,
    string: &'l str,
}
unsafe impl<'s, 'l> Transient for M<'s, 'l> {
    type Static = M<'static, 'static>;
    type Transience = ContraCo<'s, 'l>;
}

/// This function requires the first lifetime to lengthen from `'short` to
/// `'long` (*contravariance*), and the second lifetime parameter to shorten
/// from `'long` to `'short` (*covariance*). Both of these transitions are
/// allowed, and the borrow checker should *accept* it.
fn lengthen_shorten<'b, 'short, 'long: 'short>(
    short: ErasedRef<'b, ContraCo<'short, 'long>>,
) -> ErasedRef<'b, ContraCo<'long, 'short>> {
    short
}

fn main() {
    let static_str = "static";
    let temp_string = "short".to_string();

    let short_long: M<'_, 'static> = M {
        func: |_| "!",
        string: static_str,
    };
    let erased: ErasedRef<ContraCo<'_, 'static>> = short_long.erase_ref();
    let lengthened_shortened: ErasedRef<ContraCo<'static, '_>> = lengthen_shorten(erased);
    assert_eq!(lengthened_shortened.type_id(), M::static_type_id());
    drop(temp_string);
    let _ = static_str;
}
