use crate::rt::panic::ForeignExceptionType;

pub struct ForeignPanic(pub(crate) *mut ForeignExceptionType);

impl Drop for ForeignPanic {
    fn drop(&mut self) {
        unsafe { crate::panicking::lcrust::dispose_foreign_exception(self.0) }
    }
}
