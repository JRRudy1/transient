//! Tests the behavior when used on structs with no type parameters
use transient::{Any, Contra, Downcast, Inv, Transient};

#[derive(Debug, Transient)]
#[contravariant(a)]
struct FuncWrap<'a> {
    func: fn(&'a str) -> &'static str,
}

#[derive(Debug, Transient)]
#[contravariant(a)]
struct _FuncWrap<'a> {
    func: fn(&'a str) -> &'static str,
}

impl<'a> FuncWrap<'a> {
    fn call<'s: 'a>(&self, s: &'s str) -> &'static str {
        (self.func)(s)
    }
}

fn use_any_str(_s: &str) -> &'static str {
    "unrelated to the arg"
}

const STATIC_STR: &'static str = "static_str";

fn main() {
    let temp_string = "temp_str".to_string();

    // `accepts_any` is `FuncWrap<'any>`
    let accepts_any: FuncWrap<'_> = FuncWrap { func: use_any_str };

    // it can therefore be flexibly called with any string
    let _ = accepts_any.call(&temp_string);
    let _ = accepts_any.call(STATIC_STR);

    // casting to `dyn std::any::Any` requires it lengthen to `FuncWrap<'static>`,
    // so after being downcast it could only be called with a `&'static str`
    let erased: &dyn std::any::Any = &accepts_any;
    let restored: &FuncWrap<'_> = erased.downcast_ref().unwrap();
    let _ = restored.call(STATIC_STR); // OK
                                       // but the next line would NOT compile:
                                       //   let _ = restored.call(&temp_string); // Not OK

    // if we cast to `dyn transient::Any` instead, it can keep its `'any` lifetime
    // and keep accepting `&'short str` after being downcast
    let erased: &dyn Any<Contra> = &accepts_any;
    let restored: &FuncWrap<'_> = erased.downcast_ref().unwrap();
    let _ = restored.call(STATIC_STR); // OK
    let _ = restored.call(&temp_string); // Still OK

    // `Inv` also works since we don't need it to change its lifetime
    let erased: &dyn Any<Inv> = &accepts_any;
    let restored: &FuncWrap<'_> = erased.downcast_ref().unwrap();
    let _ = restored.call(STATIC_STR); // OK
    let _ = restored.call(&temp_string); // Still OK
}
