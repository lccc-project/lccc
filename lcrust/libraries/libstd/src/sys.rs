
pub(crate) macro_rules! generate_sys_crate{
    {
        $vis:vis mod $key:ident{
            $($method:ident;)*
        }
    } => {
        
        $(#[cfg_attr($key = ::core::stringify!($method), path = ::core::concat!(::core::stringify!(key),"/",::core::stringify!(),".rs"))])*
        $vis mod sys;
    }
}