#![no_main]
#![no_std]

extern "lcrust-v0" {
    fn __lccc_main() -> i32;
}

#[no_mangle]
pub unsafe extern "C" fn main() -> i32 {
    __lccc_main()
}
