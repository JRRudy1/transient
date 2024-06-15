transient
=====
This crate provides a reimplementation of the `std::any::Any` trait supporting
types with non-`'static` lifetimes.

[![Crates.io](https://img.shields.io/crates/v/transient.svg)](https://crates.io/crates/transient)

### Documentation

[Module documentation with examples](https://docs.rs/transient)

### Usage

To bring this crate into your repository, either add `transient` to your
`Cargo.toml`, or run `cargo add transient`.

Using this crate starts by implementing the provided `Transient` trait for a type,
which can be derived using the included `derive` macro or implemented by hand
by simply defining two associated types. Implementing this trait manually is 
`unsafe` but straightforward and extensively documented, and once implemented
it enables this crate's functionality to be used _safely_.

The following example demonstrates the trivial case of deriving the `Transient` 
trait for a `'static` type, and then casting it to a `dyn Any` trait object to 
emulate dynamic typing just as you would using the stdlib's implementation:

```rust
use transient::{Transient, Any, Downcast};

#[derive(Transient, Debug, PartialEq)]
struct MyUsize(usize);

fn main() {
    let orig = MyUsize(5);
    // we can erase the 'static type...
    let erased: &dyn Any = &orig;
    assert!(erased.is::<MyUsize>());
    // and restore it...
    let restored: &MyUsize = erased.downcast_ref().unwrap();
    assert_eq!(restored, &orig);
    // and use it in dynamically-typed shenanigans...
    let bananas = "bananas".to_string();
    let stuff = vec![erased, &orig.0, restored, &bananas];
    assert_eq!(stuff[0].downcast_ref::<MyUsize>().unwrap(), &orig);
}
```

Where it get's interesting is when you have a non-`'static` type containing 
borrowed data, which would be ineligable for use with the `std::any::Any`
implementation due to its `'static` bound. The following example will 
demonstrate using the `transient` crate's implementation to utilize the
same functionality as for `'static` types by simply parameterizing the 
`Any` trait by `Inv`, which is a `Transience` implementation that binds 
the lifetime and variance information that the stdlib would not be able
to handle:

```rust
use transient::{Transient, Any, Inv, Downcast};

#[derive(Transient, Debug, PartialEq)]
struct MyUsizeRef<'a>(&'a usize);

fn main() {
    let five = 5;
    let orig = MyUsizeRef(&five);
    // we can erase the 'static type...
    let erased: &dyn Any<Inv> = &orig;
    assert!(erased.is::<MyUsizeRef>());
    // and restore it...
    let restored: &MyUsizeRef = erased.downcast_ref().unwrap();
    assert_eq!(restored, &orig);
    // and use it in dynamically-typed shenanigans...
    let stuff = vec![erased, &five, restored, &"bananas"];
    assert_eq!(stuff[0].downcast_ref::<MyUsizeRef>().unwrap(), &orig);
}
```

The `Inv` type used above stands for "invariant", which is the most conservative 
form of a property known as [variance] that describes the behavior of a type 
with respect to a lifetime parameter. And understanding of variance will let 
you utilize the advanced features of this crate, but is not necessary for most 
purposes since the `Inv` type shown above be safely used for _any_ type with 
a single lifetime parameter. 

In the first example where we cast our type to a naked `dyn Any` without specifying 
a `Transience` type, the `Any` trait's default type parameter `()` was chosen
implicitly which causes it to behave like the stdlib's implementation by only 
accepting `'static` types; trying this with `MyUsizeRef` defined in the second 
example would have been rejected by the compiler. This hints at the underlying 
mechanism used to implement this crate, wherein types declare their temporal 
relationships (i.e. `Transience`) when implementing the `Transient` trait, which 
then bounds the range of `dyn Any<_>` flavors they are allowed to utilize. 
Non-`'static` types with a single lifetime `'a` that implement `Transient` using 
the derive macro are (by default) assigned a `Transience` of `Inv<'a>`, which 
limits them to being erased to (and restored from) the `dyn Any<Inv<'a>>` trait
object. By contrast, `'static` types implement the most flexible `Transience` 
of `()` which allows them to be be cast to any `dyn Any<_>` they want, up to 
and including the default `dyn Any()`. 

There is a large amount of middle-ground between these two extremes which is 
discussed in-depth throughout the documentation (hint - there are `Co` and 
`Contra` types as well), but the key takeaway is that types make a single 
`unsafe` but straight-forward decision about their temporal behavior when 
implementing the `Transient` trait, and then everything else is kept _safe_ 
using type type system and trait bounds.

### Usage: multiple life parameters

The mechanism demonstrated above extends naturally to types with more than one 
lifetime parameter by instead parameterizing the `dyn Any<_>` with a tuple as 
shown in the following example; however, the included `derive` macro currently 
only support types with zero or one lifetime parameters, so we will implement 
the `Transient` trait ourselves this time:

```rust
use transient::{Transient, Any, Inv, Downcast};

#[derive(Debug, PartialEq)]
struct TwoRefs<'a, 'b>(&'a i32, &'b i32);

unsafe impl<'a, 'b> Transient for TwoRefs<'a, 'b> {
    // the `Static` type is simply the same as the `Self` type with its 
    // lifetimes replaced by `'static`
    type Static = TwoRefs<'static, 'static>;
    // we use a tuple for the `Transience` that covers both lifetimes, using 
    // `Inv` for each element since this is always safe
    type Transience = (Inv<'a>, Inv<'b>);
}

fn main() {
    let (five, seven) = (5, 7);
    let orig = TwoRefs(&five, &seven);
    let erased: &dyn Any<Inv> = &orig;
    assert!(erased.is::<TwoRefs>());
    let restored: &TwoRefs = erased.downcast_ref().unwrap();
    assert_eq!(restored, &orig);
    let stuff = vec![erased, &five, restored, &"bananas"];
    assert_eq!(stuff[0].downcast_ref::<TwoRefs>().unwrap(), &orig);
}
```

### License

This project is licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or
   https://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or
   https://opensource.org/licenses/MIT)

at your option.

[variance]: https://doc.rust-lang.org/nomicon/subtyping.html
