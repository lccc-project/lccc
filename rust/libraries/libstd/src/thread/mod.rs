
#[cfg(thread_impl="pthread")]
#[path="pthread.rs"]
mod raw;

#[cfg(thread_impl="c11thread")]
#[path="c11thread.rs"]
mod raw;

#[cfg(thread_impl="windows")]
#[path="windows.rs"]
mod raw;


#[cfg(thread_impl="phantomos")]
#[path="phantomos"]
mod raw;


