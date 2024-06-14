//! Defines the [`Transient`] trait capturing the temporal information necessary
//! to safely erase and restore non-`'static` concrete types.
use crate::any::{Any, TypeId};
use crate::transience::Transience;

/// Unsafe trait defining the lifetime-relationships of a potentially non-`'static` 
/// type so that it can be safely erased to [`dyn Any`][crate::Any]. This trait can 
/// be derived using the [`Transient` derive macro].
///
/// # Implementing the `Transient` trait
/// Implementing the [`Transient`] trait only requires the definition of two
/// associated types: the [`Static`][Self::Static] type, and the
/// [`Transience`][Self::Transience]. The requirements for safely choosing
/// these types are explaining in the following subsections.
///
/// ## The `Static` associated type
/// This type should be the same as the `Self` type but with all lifetime
/// parameters replaced by `'static`. See the [next section][Self#Examples]
/// for numerous examples and a discussion of the few non-trivial cases such
/// types with non-`'static` generic type parameters.
///
/// ## The `Transience` associated type
/// This type must be a [`Transience`] implementation that properly captures the
/// _variance_ of the type with respect to each of its lifetime parameters. The
/// safe implementation of this types is a bit more nuanced than the `Static`
/// type, and differs depending on the number of lifetime parameters for the type
/// as laid out in the following subsections.
///
/// #### Types with 0 lifetime parameters
/// These types are the easiest to implement and use since they contain no borrowed
/// data to worry about, and can simply use the unit type `()` (or the [`Timeless`]
/// type alias) as their `Transience`.
///
/// #### Types with exactly 1 lifetime parameter
/// For a type with a single lifetime parameter `'a`, there are 3 main options
/// which correspond to the standard [variances] that a type can have:
/// - [`Inv<'a>`] -- this type declares that the implementing type is _invariant_
/// with respect to `'a`, which means that the compiler can neither shorten nor
/// length its lifetime freely. This is the most conservative form of variance,
/// and can safely be chosen for the `Transience` of any single-lifetime type.
/// However, this variance is the least flexible when it comes to usage; in a
/// world were `&'a str` was considered _invariant_ you would not be allowed
/// to pass a `&'static str` to a function expecting `&'short str`, even though
/// the former should clearly be more capable than the latter.
/// - [`Co<'a>`] -- this type declares that the implementing type is _covariant_
/// with respect to `'a`, which means that the compiler can safely shorten its
/// lifetime as needed, but cannot lengthen it. _Most_ types exhibit this variance
/// and can use it for their `Transience` (such as `&'a str` discussed above),
/// but using it in the few cases where it does _not_ apply could result in
/// undefined behavior (and so covariance cannot be the default). Notable
/// exceptions from _covariant_ behavior include the argument of a function
/// pointer (a type containing an `fn(&'a str)` is _contravariant_ w.r.t. `'a`)
/// and the pointee of a mutable reference (`&'a mut &'b str` is _invariant_
/// w.r.t. `'b`, although it is still _covariant_ w.r.t. `'a`).
/// - [`Contra<'a>`] -- this type declares that the implementing type is
/// _contravariant_ with respect to `'a`, which means that the compiler can
/// safely _lengthen_ its lifetime as needed, but cannot shorten it. This is
/// the least common variance so I won't discuss it in depth, but the main
/// example of contravariance is the relationship between a function and
/// the lifetime parameters of its arguments (such as `fn(&'a str)` as
/// mentioned above.
///
/// As a side note, a 1-tuple containing `Inv`, `Co`, or `Contra` can
/// also be used for the `Transience` of a single-lifetime type (subject
/// to the same rules as above), but this is typically less convenient
/// when it comes to usage.
///
/// #### Types with more than 1 lifetime parameter
/// Choosing a `Transience` for a type with multiple lifetime parameters
/// is really no harder than for a single-lifetime types, since the same
/// variances discussed above can simply be composed as tuples with an
/// independent element corresponding to each lifetime. For example,
/// _any_ type with two lifetimes `'a` and `'b` can safely choose
/// `(Inv<'a>, Inv<'b>)` as its `Transience` since the independent
/// choices of `Inv<'a>` and `Inv<'b>` are always safe. For a type like
/// `&'a mut &'b str` which is is _covariant_ w.r.t. to `'a` but
/// _invariant_ w.r.t. `b`, the `Transience` could be defined  as
/// `(Co<'a>, Inv<'b>)`. Just make sure to include a tuple element for
/// __every lifetime__ in the struct (choosing `Inv<'_>` for the variance
/// when unsure), since any excluded lifetimes will be unbounded and can
/// lead to undefined behavior.
///
/// Note that this pattern of composing tuples should in theory hold
/// for _any_ number of lifetimes (i.e. a type with 100 lifetimes using
/// a 100-tuple of variances), but in practice the `Transience` trait
/// is only actually implemented for 1-, 2-, 3-, and 4-tuples. If you
/// need more than this feel free to submit an issue requesting it.
///
/// # Examples
/// Note: The following examples demonstrate how to correctly implement this trait.
/// For practical usage examples, see the [crate documentation][crate#examples].
///
/// ## Static types
/// The simplest case of implementing this trait is for a struct that is already
/// `'static` (i.e. it only contains owned data and/or `'static` references. For
/// such a struct, the `Static` type can simply be `Self`, and the `Transience`
/// type can be the unit type `()` (or the type alias `transient::Timeless`):
/// ```
/// use transient::Transient;
/// struct S {
///     name: &'static str,
///     value: i32
/// }
/// unsafe impl Transient for S {
///     type Static = Self;
///     type Transience = ();
/// }
/// ```
///
/// Of course, this crate would not be necessary in this case, but it is still
/// worth mentioning that `'static` types are indeed supported.
///
/// ## Types with a single lifetime parameter
///
/// The next simplest case would be a struct with a single lifetime parameter
/// and no generic type parameters:
/// ```
/// use transient::{Transient, Inv};
/// struct S<'a> {
///     value: &'a str,
/// }
/// // This could also be derived
/// unsafe impl<'a> Transient for S<'a> {
///     type Static = S<'static>;
///     type Transience = Inv<'a>;
/// }
/// ```
///
/// ## Types with multiple lifetime parameters
///
/// Now consider a struct that borrows 2 string slices with independent
/// lifetime parameters (which is currently not supported by the derive
/// macro):
/// ```
/// struct TwoRefs<'a, 'b> {
///     a: &'a str,
///     b: &'b str,
/// }
/// ```
///
/// There are several options for how to safely implement the `Transient` trait
/// for such a type. The most versatile option is to follow the same pattern as
/// for the single-lifetime example, but to use a _tuple_ for the `Transience`
/// type that contains a separate `Transience` for each lifetime:
/// ```
/// # use transient::{Transient, Inv};
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// unsafe impl<'a, 'b> Transient for TwoRefs<'a, 'b> {
///     type Static = TwoRefs<'static, 'static>;
///     type Transience = (Inv<'a>, Inv<'b>);
/// }
/// ```
///
/// Another option is to establish a relationship between the lifetimes that allows
/// a most conservative `Transience` to be unambiguously identified for use in the impl:
/// ```
/// # use transient::{Transient, Inv};
/// # struct TwoRefs<'a, 'b> {a: &'a str, b: &'b str}
/// // 'b outlives 'a -> choose 'a for the trait
/// unsafe impl<'a, 'b: 'a> Transient for TwoRefs<'a, 'b> {
///     type Static = TwoRefs<'static, 'static>;
///     type Transience = Inv<'a>;
/// }
/// ```
///
/// This can make using the `dyn transient::Any` trait object more convenient
/// in some cases, but will result in the lifetime of the restored type being
/// truncated to the chosen lifetime.
///
/// However, choosing either `'a` **or** `'b` for the trait without declaring
/// bounds to justify the decision is *unsound* and may lead to undefined
/// behaviour.
///
/// ## Generic type parameters
///
/// Generic type parameters are also supported; however, there is one extra
/// consideration to keep in mind. Since the `Static` associated type on the
/// `Transient` trait is bounded by `'static`, any generics parameters that
/// appear in this type must also be `'static`. The easiest way to meet this
/// condition is to directly bound the type parameters by `'static` for the
/// impl block, as shown in the following example; however, there is workaround
/// for cases where this is not acceptable, which will be shown next.
///
/// For the case where the type parameters can be `'static`:
/// ```
/// use transient::{Transient, Inv};
/// // This struct is generic over type `T`, which might not be `'static`
/// struct S<'a, T> {
///     value: &'a T,
/// }
/// // By adding `T: 'static` to the impl block we can satisfy the `'static`
/// // requirement, at the cost of limiting the scope of the impl
/// unsafe impl<'a, T: 'static> Transient for S<'a, T> {
///     type Static = S<'static, T>;
///     type Transience = Inv<'a>;
/// }
/// ```
///
/// If you need to support cases where `T` is not necessarily `'static`, another
/// option is to bound `T` by `Transient` itself and then using `T::Static` in
/// the `Static` type for the impl:
/// ```
/// use transient::{Transient, Inv};
///
/// struct S<'a, T> {
///     value: &'a T,
/// }
///
/// unsafe impl<'a, T: Transient> Transient for S<'a, T> {
///     type Static = S<'static, T::Static>;
///     type Transience = Inv<'a>;
/// }
/// ```
/// Of course, this limits the impl to types where `Transient` either _is_ or
/// _can be_ implemented. If you need to support external types for which you
/// cannot implement `Transient` due to the orphan rule, your only options
/// would be to wrap `T` in a newtype struct for which you can implement
/// `Transient`, or request that the impl be added by this crate or the type's
/// crate.
/// 
/// # Safety
/// - The [`Static`][Self::Static] associated type must be the same type as the
/// implementing type, but with all lifetime parameters replaced by `'static` and 
/// any non-`'static`-bounded type parameters `T` replaced by `T::Static` (for 
/// which they must be bounded by `Transient`). Specifically, the type must have
/// the same layout as `Self` so that `std::mem::transmute` and raw pointer casts
/// pointer casts between them are sound, and the `std::any::TypeId` of the 
/// `Static` type must correctly identify the `Self` type.
/// - The [`Transience`][Self::Transience] associate type must include a component
/// for each lifetime parameter that accurately (or more conservatively) captures 
/// the `Self` type's variance with respect to it, as detailed in the documentation 
/// for the [`Transience`] trait and demonstrated in the sections above. For a 
/// `'static` type this should be `()`, for a single-lifetime type it should be 
/// [`Inv<'a>`] as a safe default or [`Co<'a>`]/[`Contra<'a>`] if appropriate, 
/// and for a multi-lifetime type this should be `(Inv<'a>, Inv<'b>, ...)` as a
/// safe default with `Co` and `Contra` optionally substituted where appropriate. 
/// Choosing `Co` or `Contra` for any lifetime parameter without respecting the 
/// rules of [Subtyping and Variance], or excluding any independent lifetime
/// parameter from the `Transience` is undefined behavior.
///
/// [`dyn Any`]: https://doc.rust-lang.org/std/any/index.html#any-and-typeid
/// [`Timeless`]: crate::transience::Timeless
/// [`Inv<'a>`]: crate::transience::Inv
/// [`Co<'a>`]: crate::transience::Co
/// [`Contra<'a>`]: crate::transience::Contra
/// [`erase`]: Transient::erase
/// [`erase_ref`]: Transient::erase_ref
/// [`erase_mut`]: Transient::erase_mut
/// [`Transient` derive macro]: transient_derive::Transient
/// [variances]: https://doc.rust-lang.org/nomicon/subtyping.html
/// [Subtyping and Variance]: https://doc.rust-lang.org/nomicon/subtyping.html
pub unsafe trait Transient: Sized {

    /// Same as `Self` but with all lifetime parameters replaced by `'static`.
    ///
    /// See the [`Transient`] trait's docstring for examples and a discussion of
    /// the considerations necessary for defining the type in various cases.
    ///
    /// # SAFETY
    /// This must be equivalent to the implementing type, such that matching its
    /// [`TypeId`] to that of a `dyn Any` trait objects is sufficient justification 
    /// for performing a [`std::mem::transmute`] or raw pointer cast to it 
    /// (excluding lifetime considerations).
    type Static: 'static;

    /// Type reflecting the variances of `Self` with respect to its lifetime parameters.
    ///
    /// See the [`Transience`] docstring for a thorough explanation and examples.
    ///
    /// # SAFETY
    /// This type must sufficiently capture the _variance_ characteristics of the 
    /// type with respect to every one of its lifetime parameters as discussed in 
    /// the documentation for the trait.
    type Transience: Transience;
    
    #[doc(hidden)]
    // attempts to validate the `Static` type and give a better error message if 
    // set incorrectly, but this is not exhaustive and must be used to take effect
    const CHECK: () = check_static_type::<Self>();

    /// Obtain the unique identifier assigned by the compiler to the 
    /// [`Static`][Self::Static] variant of the type.
    /// 
    /// See the docstring for the [`TypeId`] type for a discussion of the subtle
    /// differences from the related [`std::any::TypeId`], and the [`Any::type_id`] 
    /// method for an explanation of why this method is necessary.
    /// 
    /// See [`TypeId::of_val`] for an alternate method of obtaining the `TypeId`
    /// for a value with a concrete type.
    #[inline]
    fn static_type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }

    /// Convenience method to cast `Box<Self>` to `Box<dyn Any<_>>` with the
    /// transience defined in the `Transient` implementation.
    ///
    /// This shorthand can be useful since the default `dyn Any` only works for
    /// `'static` types, so `Transient` types would need to import the appropriate
    /// `Transience` type (such as [`Co`][crate::Co]) and explicitly specify
    /// `dyn Any<Co>` even for trivial usages (although using `dyn Any<_>` and
    /// letting type-inference fill-in-the-blank will also work in some cases).
    #[inline]
    fn erase<'a>(self: Box<Self>) -> Box<dyn Any<Self::Transience> + 'a> where Self: 'a {
        let () = Self::CHECK;
        self
    }

    /// Convenience method to cast `&Self` to `&dyn Any<_>` with the
    /// transience defined in the `Transient` implementation.
    #[inline]
    fn erase_ref<'a>(&self) -> &(dyn Any<Self::Transience> + 'a) where Self: 'a {
        let () = Self::CHECK;
        self
    }

    /// Convenience method to cast `&mut Self` to `&mut dyn Any<_>` with the
    /// transience defined in the `Transient` implementation.
    #[inline]
    fn erase_mut<'a>(&mut self) -> &mut (dyn Any<Self::Transience> + 'a) where Self: 'a {
        let () = Self::CHECK;
        self
    }
}

#[track_caller]
const fn check_static_type<T: Transient>() {
    if std::mem::size_of::<T>() != std::mem::size_of::<T::Static>() {
        panic!("Size mismatch! `T::Static` should be the same as `T` \
                but with its lifetimes replaced by `'static`")
    }
}

mod std_impls {
    use super::Transient;
    use crate::{Co, Inv};

    use std::any::Any as StdAny;
    use std::collections::HashMap;
    use std::borrow::{Cow, ToOwned};

    macro_rules! impl_refs {
        {
            $type_:ty
            [$($param:tt $(: $bound1:tt $(+ $bounds:tt)*)?),*]
            $( ($($trans:ty),+) )?
        }
        =>
        {
            #[allow(unused_parens)]
            unsafe impl<'_a, $( $param $( : $bound1 $(+ $bounds )* )? ),*>
            Transient for &'_a $type_ {
                type Static = &'static <$type_ as Transient>::Static;
                type Transience = (Co<'_a> $($(, $trans)+)?);
            }

            #[allow(unused_parens)]
            unsafe impl<'_a, $( $param $( : $bound1 $(+ $bounds )* )? ),*>
            Transient for &'_a mut $type_ {
                type Static = &'static mut <$type_ as Transient>::Static;
                type Transience = (Co<'_a> $($(, $trans)+)?);
            }

            unsafe impl<'_a, '_b, $( $param $( : $bound1 $(+ $bounds )* )? ),*>
            Transient for &'_a &'_b $type_ {
                type Static = &'static &'static <$type_ as Transient>::Static;
                type Transience = (Co<'_a>, Co<'_b> $($(, $trans)+)?);
            }

            unsafe impl<'_a, '_b, $( $param $( : $bound1 $(+ $bounds )* )? ),*>
            Transient for &'_a mut &'_b $type_ {
                type Static = &'static mut &'static <$type_ as Transient>::Static;
                type Transience = (Co<'_a>, Inv<'_b> $($(, $trans)+)?);
            }
            
            unsafe impl<'_a, '_b, $( $param $( : $bound1 $(+ $bounds )* )? ),*> 
            Transient for &'_a &'_b mut $type_ {
                type Static = &'static &'static mut <$type_ as Transient>::Static;
                type Transience = (Co<'_a>, Co<'_b> $($(, $trans)+)?);
            }
            unsafe impl<'_a, '_b, $( $param $( : $bound1 $(+ $bounds )* )? ),*> 
            Transient for &'_a mut &'_b mut $type_ {
                type Static = &'static mut &'static <$type_ as Transient>::Static;
                type Transience = (Co<'_a>, Inv<'_b> $($(, $trans)+)?);
            }
        }
    }
    use impl_refs;
    
    macro_rules! impl_primatives {
        ( $($ty:ty),* $(,)? ) => {
            $(
            unsafe impl Transient for $ty {
                type Static = $ty;
                type Transience = ();
            }
            impl_refs!($ty []);
            )*
        }
    }
    
    impl_primatives!{
        isize, i8, i16, i32, i64, i128,
        usize, u8, u16, u32, u64, u128,
        f32, f64, String, Box<str>,
    }

    unsafe impl<'a> Transient for &'a str {
        type Static = &'static str;
        type Transience = Co<'a>;
    }
    impl_refs!(&'a str ['a]);

    unsafe impl<'a, T: Transient> Transient for &'a [T] {
        type Static = &'static [T::Static];
        type Transience = Co<'a>;
    }
    impl_refs!(&'a [T] ['a, T: Transient]);

    unsafe impl<T: Transient> Transient for Vec<T> {
        type Static = Vec<T::Static>;
        type Transience = T::Transience;
    }
    impl_refs!{ Vec<T> [T: Transient] (T::Transience) }

    unsafe impl<K: Transient, V: Transient> Transient for HashMap<K, V> {
        type Static = HashMap<K::Static, V::Static>;
        type Transience = (K::Transience, V::Transience);
    }
    impl_refs!(HashMap<K, V> [K: Transient, V: Transient] (K::Transience, V::Transience));

    unsafe impl<T: Transient> Transient for Box<T> {
        type Static = Box<T::Static>;
        type Transience = T::Transience;
    }
    unsafe impl<T: Transient> Transient for Box<[T]> {
        type Static = Box<[T::Static]>;
        type Transience = T::Transience;
    }

    unsafe impl<'a, T: Transient + ToOwned> Transient for Cow<'a, T>
        where T::Static: ToOwned
    {
        type Static = Cow<'static, T::Static>;
        type Transience = (Co<'a>, T::Transience);
    }

    unsafe impl<T: Transient> Transient for Option<T> {
        type Static = Option<T::Static>;
        type Transience = T::Transience;
    }
    unsafe impl<T: Transient, E: 'static> Transient for Result<T, E> {
        type Static = Result<T::Static, E>;
        type Transience = T::Transience;
    }

    unsafe impl Transient for Box<dyn StdAny> {
        type Static = Box<dyn StdAny>;
        type Transience = ();
    }
    unsafe impl<'a> Transient for &'a dyn StdAny {
        type Static = &'static dyn StdAny;
        type Transience = Co<'a>;
    }
    unsafe impl<'a> Transient for &'a mut dyn StdAny {
        type Static = &'static mut dyn StdAny;
        type Transience = Co<'a>;
    }
}

#[cfg(feature = "ndarray")]
mod ndarray_impls {
    use ndarray::{Array, ArrayView, ArrayViewMut, ArcArray, CowArray, Dimension};

    /// Requires the `ndarray` crate feature
    unsafe impl<T, D> crate::Transient for Array<T, D>
    where
        T: 'static,
        D: Dimension + 'static,
    {
        type Static = Array<T, D>;
        type Transience = ();
    }

    /// Requires the `ndarray` crate feature
    unsafe impl<'a, T, D> crate::Transient for ArrayView<'a, T, D>
    where
        T: 'static,
        D: Dimension + 'static,
    {
        type Static = ArrayView<'static, T, D>;
        type Transience = crate::Co<'a>;
    }

    /// Requires the `ndarray` crate feature
    unsafe impl<'a, T, D> crate::Transient for ArrayViewMut<'a, T, D>
    where
        T: 'static,
        D: Dimension + 'static,
    {
        type Static = ArrayViewMut<'static, T, D>;
        type Transience = crate::Co<'a>;
    }

    /// Requires the `ndarray` crate feature
    unsafe impl<T, D> crate::Transient for ArcArray<T, D>
    where
        T: 'static,
        D: Dimension + 'static,
    {
        type Static = ArcArray<T, D>;
        type Transience = ();
    }

    /// Requires the `ndarray` crate feature
    unsafe impl<'a, T, D> crate::Transient for CowArray<'a, T, D>
    where
        T: 'static,
        D: Dimension + 'static,
    {
        type Static = CowArray<'static, T, D>;
        type Transience = crate::Co<'a>;
    }
}

#[cfg(feature = "pyo3")]
mod pyo3_impls {
    use pyo3::{Py, Bound, PyRef, PyRefMut, Borrowed};
    use pyo3::pyclass::{PyClass, boolean_struct::False};

    /// Requires the `pyo3` crate feature
    unsafe impl<T: 'static> crate::Transient for Py<T> {
        type Static = Py<T>;
        type Transience = ();
    }

    /// Requires the `pyo3` crate feature
    unsafe impl<'py, T: 'static> crate::Transient for Bound<'py, T> {
        type Static = Bound<'static, T>;
        type Transience = crate::Co<'py>;
    }

    /// Requires the `pyo3` crate feature
    unsafe impl<'a, 'py, T: 'static> crate::Transient for Borrowed<'a, 'py, T> {
        type Static = Borrowed<'static, 'static, T>;
        type Transience = (crate::Co<'a>, crate::Co<'py>);
    }

    /// Requires the `pyo3` crate feature
    unsafe impl<'py, T: PyClass> crate::Transient for PyRef<'py, T> {
        type Static = PyRef<'static, T>;
        type Transience = crate::Co<'py>;
    }

    /// Requires the `pyo3` crate feature
    unsafe impl<'py, T: PyClass<Frozen = False>> crate::Transient for PyRefMut<'py, T> {
        type Static = PyRefMut<'static, T>;
        type Transience = crate::Co<'py>;
    }
}

#[cfg(feature = "numpy")]
mod numpy_impls {

    use ndarray::Dimension;
    use numpy::{PyReadonlyArray, PyReadwriteArray, Element};

    /// Requires the `numpy` crate feature
    unsafe impl<'py, T, D> crate::Transient for PyReadonlyArray<'py, T, D>
    where
        T: Element + 'static,
        D: Dimension + 'static,
    {
        type Static = PyReadonlyArray<'static, T, D>;
        type Transience = crate::Co<'py>;
    }

    /// Requires the `numpy` crate feature
    unsafe impl<'py, T, D> crate::Transient for PyReadwriteArray<'py, T, D>
    where
        T: Element + 'static,
        D: Dimension + 'static,
    {
        type Static = PyReadwriteArray<'static, T, D>;
        type Transience = crate::Co<'py>;
    }
}
