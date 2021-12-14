
#[cfg(target_pointer_width="16")]
#[allow(non_camel_case_types)]
pub type uintptr = u16;

#[cfg(target_pointer_width="32")]
#[allow(non_camel_case_types)]
pub type uintptr = u32;

#[cfg(target_pointer_width="64")]
#[allow(non_camel_case_types)]
pub type uintptr = u64;

#[cfg(target_pointer_width="16")]
#[allow(non_camel_case_types)]
pub type intptr = i16;

#[cfg(target_pointer_width="32")]
#[allow(non_camel_case_types)]
pub type intptr = i32;

#[cfg(target_pointer_width="64")]
#[allow(non_camel_case_types)]
pub type intptr = i64;


#[cfg(target_size_width="16")]
#[allow(non_camel_case_types)]
pub type size_t = u16;

#[cfg(target_size_width="32")]
#[allow(non_camel_case_types)]
pub type size_t = u32;

#[cfg(target_size_width="64")]
#[allow(non_camel_case_types)]
pub type size_t = u64;
