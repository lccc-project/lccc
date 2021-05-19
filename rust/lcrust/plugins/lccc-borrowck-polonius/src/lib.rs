#[repr(C)]
pub struct LcrustFrontendPlugin {}

#[no_mangle]
pub extern "C" fn lcrust_init_borrowck(p: &mut LcrustFrontendPlugin) {}
