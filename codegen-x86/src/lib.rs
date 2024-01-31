use arch_ops::x86::{X86Mode, X86Register};
use target_tuples::{Architecture, Target};
use xlang::{
    abi::string::StringView, plugin::XLangCodegen, prelude::v1::*,
    targets::properties::TargetProperties,
};
use xlang_backend::{mach::Machine, SsaCodegenPlugin};

pub struct LocationAssignment {}

pub struct X86Machine {
    mode: Option<X86Mode>,
}

pub struct X86Assignments {
    mode: X86Mode,
    sp: X86Register,
    available_int_registers: Vec<X86Register>,
    stack_width: u32,
    assigns: HashMap<u32, LocationAssignment>,
}

pub struct X86Clobbers {}

impl Machine for X86Machine {
    fn matches_target(&self, targ: StringView) -> bool {
        let arch = Target::parse(&targ).arch();
        arch.is_x86() || arch == Architecture::X86_64
    }

    fn init_from_target(&mut self, properties: &TargetProperties) {
        let mode = match properties.arch.width {
            16 => X86Mode::Real,
            32 => X86Mode::Protected,
            64 => X86Mode::Long,
            _ => panic!("Not an x86 target"),
        };
        self.mode = Some(mode);
    }

    type Assignments = X86Assignments;

    type BlockClobbers = X86Clobbers;

    fn new_assignments(&self) -> Self::Assignments {
        todo!()
    }

    fn assign_locations(
        &self,
        assignments: &mut Self::Assignments,
        insns: &[xlang_backend::ssa::SsaInstruction],
        incoming: &[xlang_backend::ssa::OpaqueLocation],
        which: u32,
    ) -> Self::BlockClobbers {
        todo!()
    }

    fn codegen_block<W: arch_ops::traits::InsnWrite, F: Fn(u32) -> std::prelude::v1::String>(
        &self,
        assignments: &Self::Assignments,
        insns: &[xlang_backend::ssa::SsaInstruction],
        block_clobbers: Self::BlockClobbers,
        out: &mut W,
        label_sym: F,
        which: u32,
    ) -> std::io::Result<()> {
        todo!()
    }

    fn codegen_prologue<W: arch_ops::traits::InsnWrite>(
        &self,
        assignments: &Self::Assignments,
        out: &mut W,
    ) -> std::io::Result<()> {
        todo!()
    }
}

xlang::host::rustcall! {
#[no_mangle]
#[allow(improper_ctypes_definitions)]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(xlang::abi::boxed::Box::new(SsaCodegenPlugin::new(X86Machine{mode: None})))
}}

xlang::plugin_abi_version!("0.1");
