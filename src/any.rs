
use std::{
    any::{Any, TypeId}, mem,
};
use crate::{transient::Transient, transience::{Transience, IntoTransience, Inv, Co, Contra}, Covariant};


pub unsafe trait TrAny<R: Transience = ()>: std::fmt::Debug {
    unsafe fn into_any(self: Box<Self>) -> Box<dyn Any>;
    unsafe fn as_any(&self) -> &dyn Any;
    unsafe fn as_any_mut(&mut self) -> &mut dyn Any;

    fn static_type_id(&self) -> TypeId {
        unsafe { self.as_any().type_id() }
    }
}



unsafe impl<T, R: Transience> TrAny<R> for T
where
    T: Transient + std::fmt::Debug,
    T::Transience: IntoTransience<R>,
{
    unsafe fn into_any(self: Box<Self>) -> Box<dyn Any> {
        mem::transmute::<Box<T>, Box<T::Static>>(self)
    }
    unsafe fn as_any(&self) -> &dyn Any {
        mem::transmute::<&T, &T::Static>(self)
    }
    unsafe fn as_any_mut(&mut self) -> &mut dyn Any {
        mem::transmute::<&mut T, &mut T::Static>(self)
    }
}

pub trait Downcast<R: Transience> {
    fn downcast<T>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
        where T: Transient<Transience=R>;

    fn downcast_ref<T>(&self) -> Option<&T>
        where T: Transient<Transience=R>;

    fn downcast_mut<T>(&mut self) -> Option<&mut T>
        where T: Transient<Transience=R>;
}

impl<R: Transience> Downcast<R> for dyn TrAny<R> {

    fn downcast<T>(self: Box<Self>) -> Result<Box<T>, Box<Self>>
        where T: Transient<Transience=R>
    {
        if self.static_type_id() != T::static_type_id() {
            Err(self)
        } else {
            unsafe {
                let static_ = self.into_any().downcast::<T::Static>()
                    .expect("the `TypeId`'s were just checked");
                Ok(mem::transmute::<Box<T::Static>, Box<T>>(static_))
            }
        }
    }
    fn downcast_ref<T: Transient<Transience=R>>(&self) -> Option<&T>
        where T: Transient<Transience=R>
    {
        unsafe {
            let static_: &T::Static = self.as_any().downcast_ref()?;
            Some( mem::transmute::<&T::Static, &T>(static_) )
        }
    }
    fn downcast_mut<T: Transient<Transience=R>>(&mut self) -> Option<&mut T>
        where T: Transient<Transience=R>
    {
        unsafe {
            let static_: &mut T::Static = self.as_any_mut().downcast_mut()?;
            Some( mem::transmute::<&mut T::Static, &mut T>(static_) )
        }
    }
}


pub trait Transcend<R: Transience> {
    fn transcend<R2: Transience>(self: Box<Self>) -> Box<dyn TrAny<R2>>
        where R: IntoTransience<R2>;

    fn transcend_ref<R2: Transience>(&self) -> &dyn TrAny<R2>
        where R: IntoTransience<R2>;

    fn transcend_mut<R2: Transience>(&mut self) -> &mut dyn TrAny<R2>
        where R: IntoTransience<R2>;
}

impl<'a, R: Transience> Transcend<R> for dyn TrAny<R> + 'a {
    fn transcend<R2: Transience>(self: Box<Self>) -> Box<dyn TrAny<R2>>
        where R: IntoTransience<R2>
    {
        unsafe { mem::transmute(self) }
    }
    fn transcend_ref<R2: Transience>(&self) -> &dyn TrAny<R2>
        where R: IntoTransience<R2>
    {
        unsafe { mem::transmute(self) }
    }
    fn transcend_mut<R2: Transience>(&mut self) -> &mut dyn TrAny<R2>
        where R: IntoTransience<R2>
    {
        unsafe { mem::transmute(self) }
    }
}



#[test]
#[allow(unused)]
fn test_any() {
    use crate::{Co, Contra};

    fn f<'a, 'b: 'a>(arg1: &'a dyn TrAny<Contra<'a>>) -> &'a dyn TrAny<Inv<'b>> {
        arg1.transcend_ref()
    }

    // owned `usize`
    let value = 5_usize;
    let _: Box<dyn Any> = Box::new(5_usize);
    let _: Box<dyn TrAny> = Box::new(5_usize);
    let _: Box<dyn TrAny<()>> = Box::new(5_usize);

    // borrowed `usize`
    let _: &dyn Any = &value;
    let _: &dyn TrAny = &value;
    let _co: &dyn TrAny<Co> = &value;
    let _co: &dyn TrAny<Co> = &value;
    let _: &dyn TrAny<Inv> = _co.transcend_ref();

    // owned `&usize`
    let _: Box<dyn TrAny<Co>> = Box::new(&value);
    let _: Box<dyn TrAny<Inv>> = Box::new(&value);

    // borrowed `&usize`
    let valref = &5_usize;
    let _: &dyn TrAny<Inv> = &valref;
    let _co: &dyn TrAny<Co> = &valref;
    let _: &dyn TrAny<Inv> = _co.transcend_ref();

    // owned `&&usize`
    let _: Box<dyn TrAny<(Inv, Inv)>> = Box::new(&valref);
    let _: Box<dyn TrAny<(Co, Inv)>> = Box::new(&valref);
    let _: Box<dyn TrAny<(Co, Co)>> = Box::new(&valref);

    // borrowed `&&usize`
    let valrefref = &valref;
    let _: &dyn TrAny<(Inv, Inv)> = &valrefref;
    let _: &dyn TrAny<(Co, Inv)> = &valrefref;
    let _: &dyn TrAny<(Inv, Co)> = &valrefref;
    let _: &dyn TrAny<(Co, Co)> = &valrefref;

    // borrowed `&mut &mut usize`
    let mut mutval = 5_usize;
    let mut mutref = &mut mutval;
    let mut mutrefref = &mut mutref;

    let _: &dyn TrAny<(Inv, Inv)> = &mutrefref;
    let _: &dyn TrAny<(Co, Inv)> = &mutrefref;
    let _co: &dyn TrAny<Co> = &valref;
    let _: &dyn TrAny<Inv> = _co.transcend_ref();
    let _: &mut dyn TrAny<(Co, Inv)> = &mut mutrefref;

    let erased: Box<dyn TrAny<_>> = Box::new(5_usize);
    let restored: Box<usize> = erased.downcast().unwrap();

}

