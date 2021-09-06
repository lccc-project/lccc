
#[repr(C)]
struct ThreadHandle([u8;0]);

#[repr(C)]
struct Duration(i64,u32);

struct ThreadStartContext{
    th_stack: *mut core::ffi::c_void,
    th_internal: *mut core::ffi::c_void,
    th_start: unsafe extern"C" fn(*mut core::ffi::c_void,*mut ThreadHandle,*mut core::ffi::c_void);
}

extern"C"{
    fn GetCurrentThread() -> *mut ThreadHandle;
    fn ParkThread() -> i32;
    fn UnparkThread(th: *mut ThreadHandle) -> i32;
    fn SetBlockingTimeout(dur: &Duration);
    fn SleepThread(dur: &Duration) -> i32;
    fn JoinThread(th: *mut ThreadHandle) -> i32;
    fn InterruptThread(th: *mut ThreadHandle) -> i32;
    fn Interrupted() -> i32;
    fn StartThread(start: &ThreadStartContext,th: &mut *mut ThreadHandle) -> i32;
    fn YieldThread();
    fn ClearBlockingTimeout();
}


pub fn park(){
    ClearBlockingTimeout();
    while let ..0 = ParkThread(){}
}

pub fn park_timeout(dur: std::time::Duration){
    let mut dur1 = Duration(dur.as_secs() as i32,dur.subsec_nanos());
    let mut now = std::time::Instant::now();
    SetBlockingTimeout(&dur1);
    while {let i = ParkThread(); i!=-0x102&&i!=0}{
        let now2 = std::time::Instant::now();
        if (now2 - now)<dur{
            dur -= (now2-now);
            now = now2;
            dur1 = Duration(dur.as_secs() as i32,dur.subsec_nanos());
            SetBlockingTimeout(&dur1);
        }else{
            return;
        }
    }
}


pub fn yield_now(){
    YieldThread();
}

pub fn sleep(dur: Duration){
    ClearBlockingTimeout();
    let mut dur1 = Duration(dur.as_secs() as i32,dur.subsec_nanos());
    let mut now = std::time::Instant::now();
    while let ..0 = SleepThread(&dur1) {
        let now2 = std::time::Instant::now();
        if (now2 - now)<dur{
            dur -= (now2-now);
            now = now2;
            dur1 = Duration(dur.as_secs() as i32,dur.subsec_nanos());
        }else{
            return;
        }
    }
}
