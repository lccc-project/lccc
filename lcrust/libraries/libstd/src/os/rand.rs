

crate::sys::generate_sys_crate!{
    mod random{
        getrandom;
        winrand;
        phantomos;
        devfile;
        hardware;
        generic;
    }
}


pub fn get_random_seed() -> u64{
    sys::get_random_seed()
}