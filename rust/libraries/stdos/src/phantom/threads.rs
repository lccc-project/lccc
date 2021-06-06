
pub mod sys{
    #[repr(C)]
    pub struct ThreadHandle(::core::ffi::c_void);

    #[repr(C)]
    pub struct ProcessHandle(::core::ffi::c_void);

    #[repr(C)]
    pub struct ThreadStartContext {
        pub th_stack: *mut ::core::ffi::c_void,
        pub th_internal: *mut ::core::ffi::c_void,
        pub th_start: ::core::option::Option<
            extern "C" fn(*mut ::core::ffi::c_void, *mut ThreadHandle, *mut ::core::ffi::c_void),
        >,
    }

    #[repr(C)]
    pub struct Duration {
        pub seconds: i64,
        pub nanos: u32,
    }

    #[cfg(target_arch = "x86_64")]
    pub type SysResult = i64;
    #[cfg(all(target_arch = "x86", not(target_arch = "x86_64")))]
    pub type SysResult = i32;

    pub const SUCCESS: SysResult = 0;

    pub const THINVALID_HANDLE: SysResult = -0x100;
    pub const THINTERRUPTED: SysResult = -0x101;
    pub const THTIMEOUT: SysResult = -0x102;
    pub const THKILLED: SysResult = -0x103;

    #[allow(nonstandard_style)]
    #[cfg_attr(crtstatic,link(library="threads",kind="static"))]
    #[cfg_attr(not(crtstatic),link(library="threads"))]
    extern "C" {

        // ThreadHandle *GetCurrentThread(void);

        pub fn GetCurrentThread() -> *mut ThreadHandle;

        // _Noreturn void ExitThread(int);

        pub fn ExitThread(code: i32) -> !;

        // result StartThread(const ThreadStartContext *, ThreadHandle **);

        pub fn StartThread(ctx: *const ThreadStartContext, hdl: *mut *mut ThreadHandle) -> SysResult;

        // result ParkThread(void);

        pub fn ParkThread() -> SysResult;

        // result UnparkThread(ThreadHandle *);
        pub fn UnparkThread(hdl: *mut ThreadHandle) -> SysResult;

        // result AwaitAddress(void *);
        pub fn AwaitAddress(addr: *mut ::core::ffi::c_void) -> SysResult;

        // result SignalOne(void *);
        pub fn SignalOne(addr: *mut ::core::ffi::c_void) -> SysResult;
        // result SignalAll(void *);
        pub fn SignalAll(addr: *mut ::core::ffi::c_void) -> SysResult;

        // result SetBlockingTimeout(const duration *);

        pub fn SetBlockingTimeout(dur: *const Duration) -> SysResult;

        // result Sleep(const thread *);

        pub fn Sleep(dur: *const Duration) -> SysResult;

        // result InterruptThread(ThreadHandle *);

        pub fn InterruptThread(hdl: *mut ThreadHandle) -> SysResult;

        // result Interrupted(void);

        pub fn Interrupted() -> SysResult;

        // result JoinThread(ThreadHandle *);

        pub fn JoinThread(hdl: *mut ThreadHandle) -> SysResult;

        // result ClearBlockingTimeout()
        pub fn ClearBlockingTimeout() -> SysResult;

        // result GetThreads(ProcessHandle*,ThreadHandle**);
        pub fn GetThreads(hdl: *mut ProcessHandle, out: *mut *mut ThreadHandle) -> SysResult;
        
        pub fn GetThreadId(hdl: *const ThreadHandle) -> result;
    }
}

pub struct Thread{
    name: String,
    hdl: *mut Thread
}
