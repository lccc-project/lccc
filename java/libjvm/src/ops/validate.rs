use std::mem::take;
use crate::class::LoadedClass;

#[derive(Debug,Clone,PartialEq,Eq)]
pub enum ValidationFrameItem{
    Int,
    Float,
    Long,
    Double,
    LongExtra,
    DoubleExtra,
    Objref(LoadedClass),
    Uninit(LoadedClass),
    ReturnAddress
}

impl ValidationFrameItem{
    pub fn check_object(&self) -> Result<(),ValidationError>{
        match self{
            Self::Objref(_) => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
    pub fn check_object_of(&self,cl: &LoadedClass)->Result<(),ValidationError>{
        match self{
            Self::Objref(this) if cl.is_assignable_from(this) => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
    pub fn check_uninit(&self) -> Result<(),ValidationError>{
        match self{
            Self::Uninit(_)  => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
    pub fn check_uninit_class(&self,cl: &LoadedClass) -> Result<(),ValidationError>{
        match self{
            Self::Uninit(this) if cl.is_assignable_from(this) => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
    pub fn check_object_or_uninit(&self) -> Result<(),ValidationError>{
        match self{
            Self::Objref(_) | Self::Uninit(_) => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
    pub fn check_type(&self) -> Result<(),ValidationError>{
        match self{
            Self::LongExtra | Self::DoubleExtra | Self::ReturnAddress | Self::Uninit(_) => Err(ValidationError::BadType),
            _ => Ok(())
        }
    }
    pub fn check_array(&self) -> Result<(),ValidationError>{
        match self{
            Self::Objref(cl) if cl.is_array() => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
    pub fn check_return_address(&self) -> Result<(),ValidationError>{
        match self{
            Self::ReturnAddress => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
    pub fn check_assignable_to(&self,item: &ValidationFrameItem) -> Result<(),ValidationError> {
        match self {
            Self::Objref(cl) | Self::Uninit(cl) => item.check_object_of(cl),
            v if v == item => Ok(()),
            _ => Err(ValidationError::BadType)
        }
    }
}

#[derive(Debug,Clone)]
pub enum ValidationError{
    DoesNotExist,
    BadType,
    Linkage,
    NoSuchRef,
    OutOfBounds
}

#[derive(Debug)]
pub struct ValidationContext{
    locals: Box<[Option<ValidationFrameItem>]>,
    stack: Box<[Option<ValidationFrameItem>]>,
    stack_h: usize
}

impl ValidationContext{
    pub fn new(localc: u8,stackc: u8)->Self{
        let locals = {
            let mut locals = Vec::with_capacity(localc as usize);
            locals.resize(localc as usize,None);
            locals.into_boxed_slice()
        };
        let stack = {
            let mut stack= Vec::with_capacity(stackc as usize);
            stack.resize(stackc as usize,None);
            stack.into_boxed_slice()
        };
        Self{locals,stack,stack_h:0}
    }

    pub fn peek<F: FnOnce(&ValidationFrameItem)->Result<(),ValidationError>>(&mut self,f: F)->Result<(),ValidationError>{
        f(self.stack[self.stack_h-1].as_ref().unwrap())
    }
    pub fn pop<F: FnOnce(&ValidationFrameItem)->Result<(),ValidationError>>(&mut self,f: F)->Result<(),ValidationError>{
        if self.stack_h == 0{
            Err(ValidationError::DoesNotExist)?;
        }
        self.stack_h -=1;
        f(&(self.stack[self.stack_h].take().unwrap()))
    }
    pub fn push(&mut self,item: ValidationFrameItem) -> Result<(),ValidationError>{
        if self.stack_h == self.stack.len(){
            Err(ValidationError::OutOfBounds)?;
        }
        self.stack[self.stack_h] = Some(item);
        self.stack_h += 1;
        Ok(())
    }
    pub fn replace_head<F: FnOnce(ValidationFrameItem)->Result<ValidationFrameItem,ValidationError>>(&mut self,f: F) -> Result<(),ValidationError>{
        if self.stack_h == 0{
            Err(ValidationError::DoesNotExist)?;
        }
        let val = self.stack[self.stack_h-1].take().unwrap();
        self.stack[self.stack_h-1] = Some(f(val)?);
        Ok(())
    }

}
