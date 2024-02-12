
use std::{
    mem,
};
use crate::{
    transient::Transient,
    transience::{Transience, IntoTransience, RecoverTransience}
};
pub use std::any::{Any as StdAny, TypeId};
use std::fmt::{Formatter};


pub unsafe trait Erase<R: Transience = ()> {
    unsafe fn into_any(self: Box<Self>) -> Box<dyn StdAny>;
    unsafe fn as_any(&self) -> &dyn StdAny;
    unsafe fn as_any_mut(&mut self) -> &mut dyn StdAny;
}

pub unsafe trait Any<R: Transience = ()>: Erase<R> {

}

impl<'a, R: Transience> std::fmt::Debug for Box<dyn Any<R> + 'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tid = unsafe { self.as_any().type_id() };
        std::fmt::Debug::fmt(&format!("Box<dyn Any<_>>({:?})", tid), f)
    }
}
impl<'a, R: Transience> std::fmt::Debug for &(dyn Any<R> + 'a) {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tid = unsafe { self.as_any().type_id() };
        std::fmt::Debug::fmt(&format!("&dyn Any<_>({:?})", tid), f)
    }
}
impl<'a, R: Transience> std::fmt::Debug for &mut (dyn Any<R> + 'a) {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let tid = unsafe { self.as_any().type_id() };
        std::fmt::Debug::fmt(&format!("&mut dyn Any<_>({:?})", tid), f)
    }
}


unsafe impl<T, R: Transience> Erase<R> for T
where
    T: Transient + std::fmt::Debug,
    T::Transience: IntoTransience<R>,
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

unsafe impl<T, R> Any<R> for T
where
    R: Transience,
    T: Transient + std::fmt::Debug,
    T::Transience: IntoTransience<R>,
{}


pub trait AnyOps<'a, R: Transience> {

    fn static_type_id(&self) -> TypeId;

    fn is<T>(&self) -> bool
    where
        T: Transient<Transience=R>;
        // T::Transience: RecoverTransience<R>;

    fn downcast<T>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
    where
        T: Transient,
        T::Transience: RecoverTransience<R>;

    fn downcast_ref<T>(&self) -> Option<&T>
    where
        T: Transient,
        T::Transience: RecoverTransience<R>;

    fn downcast_mut<T>(&mut self) -> Option<&mut T>
    where
        T: Transient,
        T::Transience: RecoverTransience<R>;

    fn transcend<R2>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R2: Transience,
        R: IntoTransience<R2>;

    fn transcend_ref<R2>(&self) -> &dyn Any<R2>
    where
        R2: Transience,
        R: IntoTransience<R2>;

    fn transcend_mut<R2>(&mut self) -> &mut dyn Any<R2>
        where
        R2: Transience,
        R: IntoTransience<R2>;
}

impl<'a, R: Transience> AnyOps<'a, R> for dyn Any<R> + 'a {

    fn static_type_id(&self) -> TypeId {
        unsafe { self.as_any().type_id() }
    }

    fn is<T>(&self) -> bool
    where
        T: Transient<Transience=R>,
    {
        unsafe { self.as_any().is::<T::Static>() }
    }

    fn downcast<T>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
    where
        T: Transient,
        T::Transience: RecoverTransience<R>,
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
    fn downcast_ref<T>(&self) -> Option<&T>
    where
        T: Transient,
        T::Transience: RecoverTransience<R>,
    {
        unsafe {
            let static_: &T::Static = self.as_any().downcast_ref()?;
            Some( mem::transmute::<&T::Static, &T>(static_) )
        }
    }
    fn downcast_mut<T>(&mut self) -> Option<&mut T>
    where
        T: Transient,
        T::Transience: RecoverTransience<R>,
    {
        unsafe {
            let static_: &mut T::Static = self.as_any_mut().downcast_mut()?;
            Some( mem::transmute::<&mut T::Static, &mut T>(static_) )
        }
    }

    fn transcend<R2: Transience>(self: Box<Self>) -> Box<dyn Any<R2>>
    where
        R: IntoTransience<R2>
    {
        unsafe { mem::transmute(self) }
    }
    fn transcend_ref<R2: Transience>(&self) -> &dyn Any<R2>
    where
        R: IntoTransience<R2>
    {
        unsafe { mem::transmute(self) }
    }
    fn transcend_mut<R2: Transience>(&mut self) -> &mut dyn Any<R2>
    where
        R: IntoTransience<R2>
    {
        unsafe { mem::transmute(self) }
    }

}


#[test]
#[allow(unused)]
fn test_any<'a>() {
    use crate::{Co, Contra, Inv};

    fn f<'a, 'b: 'a>(arg1: &'a dyn Any<Contra<'a>>) -> &'a dyn Any<Inv<'b>> {
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

