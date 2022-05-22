pub use xlang_abi as __abi;

#[macro_export]
macro_rules! simple_path{
    (:: $($id:ident)::*) => {
        $crate::Path{
            components: $crate::macros::__abi::vec![$crate::PathComponent::Root, $($crate::PathComponent::Text(::core::stringify!($id))),*]
        }
    };

    ($($id:ident)::*) => {
        $crate::Path{
            components: $crate::macros::__abi::vec![$($crate::PathComponent::Text(::core::stringify!($id).into())),*]
        }
    }
}

#[macro_export]
#[doc(hidden)]
macro_rules! __parse_header_and_kind{
    ($header:expr, $kind:expr => $name:ident @ vectorsize($size:literal) $($rest:tt)+) => {
        ($header).vectorsize = $crate::macros::__abi::option::Some($size);
        $crate::__parse_header_and_kind!($header, $kind, => $name @ $($rest)+);
    };
    ($header:expr, $kind:expr => int @ min($(-$(#@$null:ident)?)? $val:literal) $($rest:tt)+) => {
        match &mut kind{
            $crate::ScalarTypeKind::Integer{min, ..} => *min = $crate::macros::__abi::option::Some($val $(^ !0 +1 $(null)?)?),
            _ => unsafe{core::hint::unreachable_unchecked()}
        }
        $crate::__parse_header_and_kind!($header, $kind, => int @ $($rest)+);
    };
    ($header:expr, $kind:expr => uint @ min($val:literal) $($rest:tt)+) => {
        match &mut kind{
            $crate::ScalarTypeKind::Integer{min, ..} => *min = $crate::macros::__abi::option::Some($val),
            _ => unsafe{core::hint::unreachable_unchecked()}
        }
        $crate::__parse_header_and_kind!($header, $kind, => uint @ $($rest)+);
    };
    ($header:expr, $kind:expr => int @ max($(-$(#@$null:ident)?)? $val:literal) $($rest:tt)+) => {
        match &mut $kind{
            $crate::ScalarTypeKind::Integer{max, ..} => *max = $crate::macros::__abi::option::Some($val $(^ !0 +1 $(null)?)?),
            _ => unsafe{core::hint::unreachable_unchecked()};
        }
        $crate::__parse_header_and_kind!($header, $kind, => int @ $($rest)+)
    };
    ($header:expr, $kind:expr => uint @ max($val:literal) $($rest:tt)+) => {
        match &mut kind{
            $crate::ScalarTypeKind::Integer{max, ..} => *max = $crate::macros::__abi::option::Some($val),
            _ => unsafe{core::hint::unreachable_unchecked()};
        }
        $crate::__parse_header_and_kind!($header, $kind => uint @ $($rest)+)
    };
    ($header:expr, $kind:expr => $name:ident @ nonzero $($rest:tt)+) => {
        ($header).validity |= $crate::ScalarValidity::NONZERO;
        $crate::__parse_header_and_kind!($header, $kind => $name @ $($rest)+);
    };
    ($header:expr, $kind:expr => float @ nonnan $($rest:tt)+) => {
        ($header).validity |= $crate::ScalarValidity::NONNAN;
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => float @ finite $($rest:tt)+) => {
        ($header).validity |= $crate::ScalarValidity::FINITE;
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => float @ decimal $($rest:tt)+) => {
        match &mut $kind{
            $crate::ScalarTypeKind::Float{decimal} => *decimal = true,
            _ => unsafe{core::hint::unreachable_unchecked()}
        }
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => lfloat @ nonnan $($rest:tt)+) => {
        ($header).validity |= $crate::ScalarValidity::NONNAN;
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => lfloat @ finite $($rest:tt)+) => {
        ($header).validity |= $crate::ScalarValidity::FINITE;
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => fixed @ fractbits($val:literal) $($rest:tt)+) => {
        match &mut $kind{
            $crate::ScalarTypeKind::Fixed{fractbits} => *fractbits = $val,
            _ => unsafe{core::hint::unreachable_unchecked()}
        }
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => char @ utf $($rest:tt)+) => {
        match &mut $kind{
            $crate::ScalarTypeKind::Char{flags} => *flags |= $crate::CharFlags::UNICODE,
            _ => unsafe{core::hint::unreachable_unchecked()}
        }
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => schar @ utf $($rest:tt)+) => {
        match &mut $kind{
            $crate::ScalarTypeKind::Char{flags} => *flags |= $crate::CharFlags::UNICODE,
            _ => unsafe{core::hint::unreachable_unchecked()}
        }
        $crate::__parse_header_and_kind!($header, $kind => float @ $($rest)+);
    };
    ($header:expr, $kind:expr => $name:ident @ ($bitsize:literal)) => {
        ($header).bitsize = $bitsize;
    }
}

#[macro_export]
macro_rules! scalar_type {
    (int $($rest:tt)+) => {

        {
            #[allow(unused_assignments)]
            let  $crate::ScalarType{mut header, mut kind} = $crate::ScalarType::default();
            kind = $crate::ScalarTypeKind::Integer{signed: true, min: $crate::macros::__abi::option::None, max: $crate::macros::__abi::option::None};
            $crate::__parse_header_and_kind!(header, kind => int @ $($rest)+);
            $crate::ScalarType{header,kind}
        }
    };

    (uint $($rest:tt)+) => {
        {
            #[allow(unused_assignments)]
            let  $crate::ScalarType{mut header, mut kind} = $crate::ScalarType::default();
            kind = $crate::ScalarTypeKind::Integer{signed: false, min: $crate::macros::__abi::option::None, max: $crate::macros::__abi::option::None};
            $crate::__parse_header_and_kind!(header, kind => uint @ $($rest)+);
            $crate::ScalarType{header,kind}
        }
    };
    (char $($rest:tt)+) => {
        {
            #[allow(unused_assignments)]
            let  $crate::ScalarType{mut header, mut kind} = $crate::ScalarType::default();
            kind = $crate::ScalarTypeKind::Char{flags: $crate::CharFlags::empty()};
            $crate::__parse_header_and_kind!(header, kind => char @ $($rest)+);
            $crate::ScalarType{header,kind}
        }
    };
    (schar $($rest:tt)+) => {
        {
            #[allow(unused_assignments)]
            let $crate::ScalarType{mut header, mut kind} = $crate::ScalarType::default();
            kind = $crate::ScalarTypeKind::Char{flags: $crate::CharFlags::SIGNED};
            $crate::__parse_header_and_kind!(header, kind => schar @ $($rest)+);
            $crate::ScalarType{header,kind}
        }
    };
    (float $($rest:tt)+) => {
        {
            #[allow(unused_assignments)]
            let  $crate::ScalarType{mut header, mut kind} = $crate::ScalarType::default();
            kind = $crate::ScalarTypeKind::Float{decimal: false};
            $crate::__parse_header_and_kind!(header, kind => float @ $($rest)+);
            $crate::ScalarType{header,kind}
        }
    };
    (lfloat $($rest:tt)+) => {
        {
            #[allow(unused_assignments)]
            let  $crate::ScalarType{mut header, mut kind} = $crate::ScalarType::default();
            kind = $crate::ScalarTypeKind::LongFloat;
            $crate::__parse_header_and_kind!(header, kind => float @ $($rest)+);
            $crate::ScalarType{header,kind}
        }
    };
    (fixed $($rest:tt)+) => {
        {
            #[allow(unused_assignments)]
            let  $crate::ScalarType{mut header, mut kind} = $crate::ScalarType::default();
            kind = $crate::ScalarTypeKind::Fixed{fractbits: 0};
            $crate::__parse_header_and_kind!(header, kind => float @ $($rest)+);
            $crate::ScalarType{header,kind}
        }
    };
}
