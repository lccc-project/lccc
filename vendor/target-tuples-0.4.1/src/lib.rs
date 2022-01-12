#![deny(warnings, unsafe_code)]
#![cfg_attr(not(any(doc, test)), no_std)]

extern crate alloc;

mod pieces;

pub use pieces::*;

#[doc(hidden)]
#[macro_export]
macro_rules! __to_target {
    ($first:ident-$($rest:ident)-+) => {
        ::core::concat!(::core::stringify!($first) $(,"-",::core::stringify!($rest))*)
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! __match_target_pattern {
    ($arch:ident-$vendor:ident-$os:ident-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!($arch - $vendor - $os - $env));
            *targ == mtarg
        }

        __check
    }};

    ($arch:ident-$vendor:ident-$sys:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!($arch - $vendor - $sys));
            targ == mtarg
        }

        __check
    }};

    (*-$vendor:ident-$os:ident-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!(x86_64 - $vendor - $os - $env));

            targ.get_vendor() == targ.get_vendor()
                && targ.get_operating_system() == mtarg.get_operating_system()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};
    (*-$vendor:ident-$sys:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!(x86_64 - $vendor - $sys));

            targ.get_vendor() == targ.get_vendor()
                && targ.get_operating_system() == mtarg.get_operating_system()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};

    ($arch:ident-*-$os:ident-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!($arch - unknown - $os - $env));

            targ.get_arch() == targ.get_arch()
                && targ.get_operating_system() == mtarg.get_operating_system()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};
    ($arch:ident-*-$sys:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!($arch - unknown - $sys));

            targ.get_arch() == targ.get_arch()
                && targ.get_operating_system() == mtarg.get_operating_system()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};

    ($arch:ident-$vendor:ident-*-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg =
                $crate::Target::parse($crate::__to_target!($arch - $vendor - unknown - $env));

            targ.get_arch() == targ.get_arch()
                && targ.get_vendor() == mtarg.get_vendor()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};

    ($arch:ident-$vendor:ident-*) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!($arch - $vendor - elf));

            targ.get_arch() == targ.get_arch() && targ.get_vendor() == mtarg.get_vendor()
        }

        __check
    }};

    ($arch:ident-$vendor:ident-$os:ident-*) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!($arch - $vendor - $os - elf));

            targ.get_vendor() == mtarg.get_vendor()
                && targ.get_operating_system() == mtarg.get_operating_system()
        }

        __check
    }};

    (*-*-$os:ident-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!(x86_64 - unknown - $os - $env));

            targ.get_operating_system() == mtarg.get_operating_system()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};
    (*-*-$sys:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!(x86_64 - unknown - $sys));

            targ.get_operating_system() == mtarg.get_operating_system()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};

    (*-$vendor:ident-*-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg =
                $crate::Target::parse($crate::__to_target!(x86_64 - $vendor - unknown - $env));

            targ.get_vendor() == mtarg.get_vendor()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};

    (*-$vendor:ident-*) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!(x86_64 - $vendor - elf));

            targ.get_vendor() == mtarg.get_vendor()
        }

        __check
    }};

    (*-$vendor:ident-$os:ident-*) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!(x86_64 - $vendor - $os - elf));

            targ.get_vendor() == mtarg.get_vendor()
                && targ.get_operating_system() == mtarg.get_operating_system()
        }

        __check
    }};

    ($arch:ident-*-*-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg =
                $crate::Target::parse($crate::__to_target!($arch - unknown - unknown - $env));

            targ.get_arch() == targ.get_arch()
                && targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};

    ($arch:ident-*-*) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg = $crate::Target::parse($crate::__to_target!($arch - unknown - elf));

            targ.get_arch() == targ.get_arch()
        }

        __check
    }};

    ($arch:ident-*-$os:ident-*) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg =
                $crate::Target::parse($crate::__to_target!($arch - unknown - $os - unknown));

            targ.get_arch() == mtarg.get_arch()
                && targ.get_operating_system() == mtarg.get_operating_system()
        }

        __check
    }};

    (*-*-*-$env:ident) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg =
                $crate::Target::parse($crate::__to_target!(x86_64 - unknown - unknown - $env));

            targ.get_environment() == mtarg.get_environment()
                && targ.get_object_format() == mtarg.get_object_format()
        }

        __check
    }};

    (*-*-$os:ident-*) => {{
        fn __check(targ: &$crate::Target) -> bool {
            let mtarg =
                $crate::Target::parse($crate::__to_target!(x86_64 - unknown - $os - unknown));

            targ.get_operating_system() == mtarg.get_operating_system()
        }

        __check
    }};

    (*-*-*) => {{
        fn __check(_: &$crate::Target) -> bool {
            true
        }
        __check
    }};

    (*) => {{
        fn __check(_: &$crate::Target) -> bool {
            true
        }
        __check
    }};
}

#[macro_export]
macro_rules! match_targets{
    {
        match ($targ:expr) {
            $($($comp:tt)-* => $exp:expr),* $(,)?
        }
    } => {
        {
            let __val = $targ;
            #[allow(unreachable_code)]
            loop {
                $(if ($crate::__match_target_pattern!($($comp)-*))(&__val){
                    break $exp
                })*

                panic!("Incomplete Exhaustive Target Pattern (add a wildcard match as * => )")
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::Target;

    #[test]
    pub fn test_match_easy() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        match_targets! {
            match (target) {
                x86_64-pc-linux-gnu => {},
                * => panic!("Invalid Target")
            }
        }
    }

    #[test]
    pub fn target_match_arch_wildcard() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        match_targets! {
            match (target) {
                *-pc-linux-gnu => {},
                * => panic!("Invalid Target")
            }
        }
    }

    #[test]
    pub fn target_match_vendor_wildcard() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        match_targets! {
            match (target) {
                x86_64-*-linux-gnu => {},
                * => panic!("Invalid Target")
            }
        }
    }

    #[test]
    pub fn target_match_os_wildcard() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        match_targets! {
            match (target) {
                x86_64-pc-*-gnu => {},
                * => panic!("Invalid Target")
            }
        }
    }

    #[test]
    pub fn target_match_env_wildcard() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        match_targets! {
            match (target) {
                x86_64-pc-linux-* => {},
                * => panic!("Invalid Target")
            }
        }
    }

    #[test]
    pub fn target_match_sys_wildcard() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        match_targets! {
            match (target) {
                x86_64-pc-* => {},
                * => panic!("Invalid Target")
            }
        }
    }

    #[test]
    pub fn target_match_first() {
        let target = Target::parse("x86_64-pc-linux-gnu");
        match_targets! {
            match (target) {
                x86_64-pc-linux-gnu => {},
                x86_64-*-linux-* => panic!("Incorrect Match"),
                * => panic!("Incorrect Match"),
            }
        }
    }
}
