use target_tuples::{Architecture, Target};
use xlang::{
    abi::string::StringView, plugin::XLangCodegen, prelude::v1::*,
    targets::properties::TargetProperties,
};
use xlang_backend::{mach::Machine, SsaCodegenPlugin};

pub struct W65Assignments {}

pub struct W65Clobbers {}

pub struct W65Machine {}

impl Machine for W65Machine {
    fn matches_target(&self, targ: StringView) -> bool {
        Target::parse(&targ).arch() == Architecture::Wc65c816
    }

    fn init_from_target(&mut self, _: &TargetProperties) {}

    type Assignments = W65Assignments;

    type BlockClobbers = W65Clobbers;

    fn new_assignments(&self) -> Self::Assignments {
        W65Assignments {}
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

    fn codegen_prologue<W: arch_ops::traits::InsnWrite>(
        &self,
        assignments: &Self::Assignments,
        out: &mut W,
    ) -> std::io::Result<()> {
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
}

xlang::host::rustcall! {
#[no_mangle]
#[allow(improper_ctypes_definitions)]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(xlang::abi::boxed::Box::new(SsaCodegenPlugin::new(W65Machine{})))
}}

xlang::plugin_abi_version!("0.1");
