use arch_ops::x86::X86Register;
use target_tuples::Target;
use xlang::abi::{span, span::Span};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct X86CallConv {
    pub intregs: Span<'static, X86Register>,
    pub floatregs: Span<'static, X86Register>,
    pub vecregs: Span<'static, X86Register>,
    pub saveregs: Span<'static, X86Register>,
    pub callee_clean: bool,
    pub stack_ltr_order: bool,
    pub struct_always_on_stack: bool,
    pub iretregs: Span<'static, X86Register>,
    pub fretregs: Span<'static, X86Register>,
    pub vretregs: Span<'static, X86Register>,
    pub redzone: u16,
    pub shadow: u16,
    pub salign: u16,
}

pub static SYSV64: X86CallConv = X86CallConv {
    intregs: span![
        X86Register::Rdi,
        X86Register::Rsi,
        X86Register::Rdx,
        X86Register::Rcx,
        X86Register::R8,
        X86Register::R9
    ],
    floatregs: span![
        X86Register::Xmm(0),
        X86Register::Xmm(1),
        X86Register::Xmm(2),
        X86Register::Xmm(3),
        X86Register::Xmm(4),
        X86Register::Xmm(5),
        X86Register::Xmm(6),
        X86Register::Xmm(7)
    ],
    vecregs: span![
        X86Register::Xmm(0),
        X86Register::Xmm(1),
        X86Register::Xmm(2),
        X86Register::Xmm(3),
        X86Register::Xmm(4),
        X86Register::Xmm(5),
        X86Register::Xmm(6),
        X86Register::Xmm(7)
    ],
    saveregs: span![
        X86Register::Rbx,
        X86Register::Rsp,
        X86Register::Rbp,
        X86Register::R12,
        X86Register::R13,
        X86Register::R14,
        X86Register::R15
    ],
    callee_clean: false,
    stack_ltr_order: false,
    struct_always_on_stack: false,
    iretregs: span![X86Register::Rax, X86Register::Rdx],
    fretregs: span![X86Register::Xmm(0), X86Register::Xmm(1)],
    vretregs: span![X86Register::Xmm(0), X86Register::Xmm(1)],
    redzone: 128,
    shadow: 0,
    salign: 16,
};

#[must_use]
///
/// # Panics
/// if the target is not a known x86 target
pub fn get_calling_convention(tag: xlang::ir::Abi, target: &Target) -> &'static X86CallConv {
    target_tuples::match_targets! {
        match (target){
            x86_64-*-windows-msvc => match tag{
                xlang_struct::Abi::Vectorcall => todo!("vectorcall"),
                _ => todo!("win64")
            },
            x86_64-*-* => &SYSV64,

            * => panic!("Unknown target")
        }
    }
}
