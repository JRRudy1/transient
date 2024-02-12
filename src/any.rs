
use std::{mem, fmt};
use crate::{
    transient::Transient,
    transience::{Transience, CanRecoverFrom, CanTranscendTo}
};

pub use std::any::{Any as StdAny, TypeId};

/// Main `Any` trait for use as a trait object.
pub unsafe trait Any<R: Transience = ()>: Erase<R> {
    fn static_type_id(&self) -> TypeId;
}
/// Alternative namespace for `Any`. This could be merged with `Any`, but
/// hiding these unsafe methods from the `Any` namespace seems better.
pub unsafe trait Erase<R: Transience = ()> {
    unsafe fn into_any(self: Box<Self>) -> Box<dyn StdAny>;
    unsafe fn as_any(&self) -> &dyn StdAny;
    unsafe fn as_any_mut(&mut self) -> &mut dyn StdAny;
}


unsafe impl<T, R> Any<R> for T
where
    R: Transience,
    T: Transient,
    T::Transience: CanTranscendTo<R>
{
    fn static_type_id(&self) -> TypeId {
        TypeId::of::<T::Static>()
    }
}

unsafe impl<T, R> Erase<R> for T
where
    R: Transience,
    T: Transient,
    T::Transience: CanTranscendTo<R>
{
    unsafe fn into_any(self: Box<Self>) -> Box<dyn StdAny> {
        mem::transmute::<Box<T>, Box<T::Static>>(self)
    }
    unsafe fn as_any(&self) -> &dyn StdAny {
        mem::transmute::<&T, &T::Static>(self)
    }
    unsafe fn as_any_mut(&mut self) -> &mut dyn StdAny {
        mem::transmute::<&mut T, &mut T::Static>(self)
    }
}


/// Extension trait used to define methods on the `dyn Any<_>` trait object.
pub trait AnyOps<R: Transience> {

    fn is<T: Transient>(&self) -> bool;

    fn downcast<T: Transient>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
        where T::Transience: CanRecoverFrom<R>;

    fn downcast_ref<T: Transient>(&self) -> Option<&T>
        where T::Transience: CanRecoverFrom<R>;

    fn downcast_mut<T: Transient>(&mut self) -> Option<&mut T>
        where T::Transience: CanRecoverFrom<R>;

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
        where R: CanTranscendTo<R2>;

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend_ref<R2: Transience>(&self) -> &dyn Any<R2>
        where R: CanTranscendTo<R2>;

    /// Upcast to another `Transience` in compliance with sub-typing relationships.
    fn transcend_mut<R2>(&mut self) -> &mut dyn Any<R2>
        where R: CanTranscendTo<R2>;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`.
    unsafe fn transcend_unbounded<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
        where R2: Transience;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`.
    unsafe fn transcend_ref_unbounded<R2>(&self) -> &dyn Any<R2>
        where R2: Transience;

    /// Cast to another `Transience` without enforcing sub-typing relationships.
    ///
    /// # Safety
    /// An invalid transition must not be performed unless external invariants
    /// guarantee that the type is valid for the new `Transience`.
    unsafe fn transcend_mut_unbounded<R2>(&mut self) -> &mut dyn Any<R2>
        where R2: Transience;
}

impl<'a, R> AnyOps<R> for (dyn Any<R> + 'a)
where
    R: Transience
{
    /// Returns `true` if the inner type is the same as `T::Static`.
    fn is<T: Transient>(&self) -> bool {
        self.static_type_id() == TypeId::of::<T::Static>()
    }

    fn downcast<T: Transient>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
    where
        T::Transience: CanRecoverFrom<R>,
    {
        if self.static_type_id() != TypeId::of::<T::Static>() {
            Err(self)
        } else {
            unsafe {
                let static_ = self.into_any().downcast::<T::Static>()
                    .expect("the `TypeId`'s were just checked");
                Ok(mem::transmute::<Box<T::Static>, Box<T>>(static_))
            }
        }
    }
    fn downcast_ref<T: Transient>(&self) -> Option<&T>
    where
        T::Transience: CanRecoverFrom<R>,
    {
        unsafe {
            let static_: &T::Static = self.as_any().downcast_ref()?;
            Some( mem::transmute::<&T::Static, &T>(static_) )
        }
    }
    fn downcast_mut<T: Transient>(&mut self) -> Option<&mut T>
    where
        T::Transience: CanRecoverFrom<R>,
    {
        unsafe {
            let static_: &mut T::Static = self.as_any_mut().downcast_mut()?;
            Some( mem::transmute::<&mut T::Static, &mut T>(static_) )
        }
    }
    fn transcend<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R: CanTranscendTo<R2>
    {
        unsafe { mem::transmute(self) }
    }
    fn transcend_ref<R2>(&self) -> &dyn Any<R2>
    where
        R: CanTranscendTo<R2>
    {
        unsafe { mem::transmute(self) }
    }
    fn transcend_mut<R2>(&mut self) -> &mut dyn Any<R2>
    where
        R: CanTranscendTo<R2>
    {
        unsafe { mem::transmute(self) }
    }
    unsafe fn transcend_unbounded<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R2: Transience
    {
        unsafe { mem::transmute(self) }
    }
    unsafe fn transcend_ref_unbounded<R2: Transience>(&self) -> &dyn Any<R2>
    where
        R2: Transience
    {
        unsafe { mem::transmute(self) }
    }
    unsafe fn transcend_mut_unbounded<R2>(&mut self) -> &mut dyn Any<R2>
    where
        R2: Transience
    {
        unsafe { mem::transmute(self) }
    }
}

impl<'a, R: Transience> fmt::Debug for Box<dyn Any<R> + 'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tid = unsafe { self.as_any().type_id() };
        std::fmt::Debug::fmt(&format!("Box<dyn Any<_>>({:?})", tid), f)
    }
}
impl<'a, R: Transience> fmt::Debug for &(dyn Any<R> + 'a) {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tid = unsafe { self.as_any().type_id() };
        std::fmt::Debug::fmt(&format!("&dyn Any<_>({:?})", tid), f)
    }
}
impl<'a, R: Transience> fmt::Debug for &mut (dyn Any<R> + 'a) {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tid = unsafe { self.as_any().type_id() };
        std::fmt::Debug::fmt(&format!("&mut dyn Any<_>({:?})", tid), f)
    }
}



#[test]
#[allow(unused)]
fn test_any<'a>() {
    use crate::{Co, Contra, Inv};

    fn f<'a, 'b: 'a>(arg1: &'a dyn Any<Contra<'a>>) -> &'a dyn Any<Inv<'b>> {
        // R                  R2
        // Contra<'short> into Inv<'long>
        // Contra<'short>: SubTransience<Inv<'long>>
        // Inv<'long>: SuperTransience<Contra<'short>>
        // R: SubTransience<R2>
        // R2: SuperTransience<R>

        arg1.transcend_ref()
    }
    #[derive(Debug)]
    pub struct Usize(usize);

    #[derive(Debug, Clone, Copy)]
    pub struct UsizeRef<'a>(&'a usize);
    #[derive(Debug, Clone, Copy)]
    pub struct UsizeRefRef<'a, 'b>(&'a &'b usize);

    unsafe impl Transient for Usize {
        type Static = Usize;
        type Transience = ();
    }
    unsafe impl<'a> Transient for UsizeRef<'a> {
        type Static = UsizeRef<'static>;
        type Transience = Co<'a>;
    }
    unsafe impl<'a, 'b> Transient for UsizeRefRef<'a, 'b> {
        type Static = UsizeRefRef<'static, 'static>;
        type Transience = (Co<'a>, Co<'b>);
    }

    // owned `usize`
    let value = 5_usize;
    let _: &dyn StdAny = &5_usize;

    let _: Box<dyn StdAny> = Box::new(5_usize);
    // let _: Box<dyn TrAny> = Box::new(5_usize);
    let _: Box<dyn Any<()>> = Box::new(5_usize);

    // borrowed `usize`
    let _: &dyn StdAny = &value;
    let _: &dyn Any = &value;
    let _co: &dyn Any<Co> = &value;
    let _: &dyn Any<Inv> = _co.transcend_ref();
    let _: &usize = _co.downcast_ref().unwrap();

    // owned `&usize`
    let r: &usize = &value;
    let x: Box<dyn Any<Inv>> = Box::new(r);
    // let _: Box<dyn Any<Inv>> = Box::new(&value);
    let y: Box<&usize> = x.downcast().unwrap();
    // let y: Box<&usize> = x.downcast::<&'_ usize>().unwrap();

    // borrowed `&usize`
    let valref = &5_usize;
    let _: &dyn Any<Inv> = &valref;
    let _co: &dyn Any<Co> = &valref;
    let _dc: &&usize = _co.downcast_ref().unwrap();

    // let _: &dyn Any<Inv> = _co.transcend_ref();

    // owned `&&usize`
    let _: Box<dyn Any<(Inv, Inv)>> = Box::new(&valref);
    let _: Box<dyn Any<(Co, Inv)>> = Box::new(&valref);
    let _: Box<dyn Any<(Co, Co)>> = Box::new(&valref);

    // borrowed `&&usize`
    let valrefref = &valref;
    let _: &dyn Any<(Inv, Inv)> = &valrefref;
    let _: &dyn Any<(Co, Inv)> = &valrefref;
    let _: &dyn Any<(Inv, Co)> = &valrefref;
    let _: &dyn Any<(Co, Co)> = &valrefref;

    // borrowed `&mut &mut usize`
    let mut mutval = 5_usize;
    let mut mutref = &mut mutval;
    let mut mutrefref = &mut mutref;

    let _: &dyn Any<(Inv, Inv)> = &mutrefref;
    let _: &dyn Any<(Co, Inv)> = &mutrefref;
    let _co: &dyn Any<Co> = &valref;
    let _: &dyn Any<Inv> = _co.transcend_ref();
    let _: &mut dyn Any<(Co, Inv)> = &mut mutrefref;

    let erased: Box<dyn Any> = Box::new(5_usize);
    let _: Box<usize> = erased.downcast().unwrap();

    let erased: &dyn Any = &value;
    let _: &usize = erased.downcast_ref().unwrap();

}

