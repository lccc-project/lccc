pub struct TypeId(*const u8, usize);

use core::{
    cmp::{Eq, Ord, Ordering, PartialEq, PartialOrd},
    hash::{Hash, Hasher},
};

impl PartialEq for TypeId {
    fn eq(&self, other: &TypeId) -> bool {
        if core::ptr::eq(self.0, other.0) {
            unsafe { ::__lccc::builtins::C::__builtin_assume(self.1 == other.1) }
            true
        } else if self.1 != other.1 {
            false
        } else {
            let mut p0 = self.0;
            let mut p1 = other.0;
            // Safety:
            // both p0 and p1 point to Null Terminated Multibyte Strings according to the ABI
            // So before the loop passes the end of the allocation, it will read a null terminator in either or both
            unsafe {
                loop {
                    if *p0 == 0 || *p1 == 0 {
                        return *p1 == *p0;
                    } else if *p0 != *p1 {
                        return false;
                    } else {
                        p0 = p0.offset(1);
                        p1 = p1.offset(1);
                    }
                }
            }
        }
    }
}

impl PartialOrd for TypeId {
    fn partial_cmp(&self, other: &TypeId) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for TypeId {
    fn cmp(&self, other: &TypeId) -> Ordering {
        let mut p0 = self.0;
        let mut p1 = other.0;
        // Safety:
        // both p0 and p1 point to Null Terminated Multibyte Strings according to the ABI
        // So before the loop passes the end of the allocation, it will read a null terminator in either or both
        unsafe {
            loop {
                if *p0 != *p1 {
                    return (*p0).cmp(&*p1);
                } else if *p0 == 0 {
                    return Ordering::Equal;
                } else {
                    p0 = p0.offset(1);
                    p1 = p1.offset(1);
                }
            }
        }
    }
}

impl Eq for TypeId {}

impl Hash for TypeId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_usize(self.0)
    }
}

impl core::fmt::Debug for TypeId{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result{
        let mut end = self.1;
        while unsafe{*end != 0} {end.offset(1);}
        let st = unsafe{core::str::from_utf8_unchecked(core::slice::from_pointer_range(self.1..end))};
        f.debug_struct("TypeId")
            .field("name",&st)
            .field("hash",&self.0)
            .finish()
    }
}

impl TypeId {
    pub fn of<T: ?Sized + 'static>() -> Self {
        let (a, b) = ::__lccc::builtins::Rust::__builtin_typeid::<T>();
        Self(a, b)
    }
}

use core::marker::{Send, Sync};

unsafe impl Send for TypeId {}
unsafe impl Sync for TypeId {}

pub trait Any: 'static {
    fn type_id(&self) -> TypeId;
}

impl<T: ?Sized + 'static> Any for T {
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
}

impl dyn Any {
    pub fn is<T: Any>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            // Safety:
            // The type of self has been verified above
            // &self is valid for lifetime of returned reference
            Some(unsafe { &*(self as *const dyn Any as *const T) })
        } else {
            None
        }
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            // Safety:
            // The type of self has been verified above
            // &self is valid for lifetime of returned reference
            Some(unsafe { &mut *(self as *mut dyn Any as *mut T) })
        } else {
            None
        }
    }
}
impl dyn Any + Send {
    pub fn is<T: Any>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            // Safety:
            // The type of self has been verified above
            // &self is valid for lifetime of returned reference
            Some(unsafe { &*(self as *const (dyn Any + Send) as *const T) })
        } else {
            None
        }
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            // Safety:
            // The type of self has been verified above
            // &self is valid for lifetime of returned reference
            Some(unsafe { &mut *(self as *mut (dyn Any + Send) as *mut T) })
        } else {
            None
        }
    }
}

impl dyn Any + Send + Sync {
    pub fn is<T: Any>(&self) -> bool {
        TypeId::of::<T>() == self.type_id()
    }

    pub fn downcast_ref<T: Any>(&self) -> Option<&T> {
        if self.is::<T>() {
            // Safety:
            // The type of self has been verified above
            // &self is valid for lifetime of returned reference
            Some(unsafe { &*(self as *const (dyn Any + Send + Sync) as *const T) })
        } else {
            None
        }
    }

    pub fn downcast_mut<T: Any>(&mut self) -> Option<&mut T> {
        if self.is::<T>() {
            // Safety:
            // The type of self has been verified above
            // &self is valid for lifetime of returned reference
            Some(unsafe { &mut *(self as *mut (dyn Any + Send + Sync) as *mut T) })
        } else {
            None
        }
    }
}
