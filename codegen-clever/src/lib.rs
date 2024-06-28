use arch_ops::clever::CleverInstruction;
use target_tuples::{Architecture, Target};
use xlang::{
    abi::string::StringView, plugin::XLangCodegen, prelude::v1::*,
    targets::properties::TargetProperties,
};
use xlang_backend::{
    mach::{
        mce::{MceInstruction, MceWriter},
        Machine,
    },
    ssa::{OpaqueLocation, SsaInstruction},
    ty::TypeInformation,
    SsaCodegenPlugin,
};

pub struct CleverAssignments {}

pub struct CleverClobbers {}

pub struct CleverMachine {}

impl MceWriter for CleverMachine {
    type Instruction = CleverInstruction;

    fn write_assembly<W: core::fmt::Write>(
        &self,
        insn: &[MceInstruction<Self::Instruction>],
        writer: &mut W,
    ) -> core::fmt::Result {
        todo!()
    }

    fn write_machine_code<W: arch_ops::traits::InsnWrite, F: FnMut(u128, std::string::String)>(
        &self,
        insn: &[MceInstruction<Self::Instruction>],
        writer: &mut W,
        sym_accepter: &mut F,
    ) -> std::io::Result<()> {
        todo!()
    }
}

impl Machine<SsaInstruction> for CleverMachine {
    fn matches_target(&self, targ: StringView) -> bool {
        Target::parse(&targ).arch() == Architecture::Clever
    }

    fn init_from_target(&mut self, _: &TargetProperties) {}

    type Assignments = CleverAssignments;

    type BlockClobbers = CleverClobbers;

    fn new_assignments(&self) -> Self::Assignments {
        todo!()
    }

    fn assign_locations(
        &self,
        assignments: &mut Self::Assignments,
        insns: &[xlang_backend::ssa::SsaInstruction],
        incoming: &[xlang_backend::ssa::OpaqueLocation],
        which: u32,
        incoming_set: &HashMap<u32, xlang::abi::vec::Vec<OpaqueLocation>>,
        tys: &TypeInformation,
    ) -> Self::BlockClobbers {
        todo!()
    }

    fn codegen_prologue(
        &self,
        assignments: &Self::Assignments,
    ) -> Vec<MceInstruction<CleverInstruction>> {
        todo!()
    }

    fn codegen_block<F: Fn(u32) -> std::prelude::v1::String>(
        &self,
        assignments: &Self::Assignments,
        insns: &[xlang_backend::ssa::SsaInstruction],
        block_clobbers: Self::BlockClobbers,
        label_sym: F,
        which: u32,
        tys: &TypeInformation,
    ) -> Vec<MceInstruction<CleverInstruction>> {
        todo!()
    }

    fn assign_call_conv(
        &self,
        assignments: &mut Self::Assignments,
        incoming: &[OpaqueLocation],
        fnty: &xlang_struct::FnType,
        tys: &TypeInformation,
        which: u32,
    ) {
        todo!()
    }
}

xlang::host::rustcall! {
#[no_mangle]
#[allow(improper_ctypes_definitions)]
pub extern "rustcall" fn xlang_backend_main() -> DynBox<dyn XLangCodegen> {
    DynBox::unsize_box(xlang::abi::boxed::Box::new(SsaCodegenPlugin::new(CleverMachine{})))
}}

xlang::plugin_abi_version!("0.1");
