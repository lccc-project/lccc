use arch_ops::traits::InsnWrite;
use xlang::abi::vec::Vec;

/// A Machine code instruction or psuedo-instruction
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MceInstruction<I> {
    /// The base instruction type
    BaseInsn(I),
    /// Indicates that no analysis should be performed on subsequent instructions until reaching an [`EnableAnalyze`][MceInstruction::EnableAnalyze]
    DisableAnalyze,
    /// Overrides the disposition of a previous [`DisableAnalyze`][MceInstruction::DisableAnalyze]
    EnableAnalyze,
    /// A MCE-context private label.
    /// Primarily used by assembly, but some `ssa` instructions that lower to hardware branches may use this as well
    ///
    /// The codegen impl is responsible for appropriately naming the label symbol, but the general syntax should be `<bb-label>._X<p-label-no>`
    PrivateLabel(String),
    /// Raw bytes being generated as part of the function
    /// Assembly directives that emit raw bytes, including `.insn`, will use this variant
    RawBytes(Vec<u8>),
    /// Placeholder for Legalizer/Opt that an instruction that was present was eliminated by the transform
    Empty,
    /// Placeholder for Legalizer/Opt that a single instruction was split into multiple
    Split(Vec<MceInstruction<I>>),
}

/// A trait for types that can write a sequence of instructions to either a relocatable byte stream or textual assembly
pub trait MceWriter {
    /// The instruction type for the Mce Codegen
    type Instruction;
    /// Writes a sequence of [`MceInstruction`]s to an [`InsnWrite`]
    fn write_machine_code<W: InsnWrite, F: FnMut(u128, String)>(
        &self,
        insn: &[MceInstruction<Self::Instruction>],
        writer: &mut W,
        sym_accepter: &mut F,
    ) -> std::io::Result<()>;
    /// Writes the assembly syntax form of a sequence of [`MceInstruction`]s to a [`fmt::Write`][core::fmt::Write]
    fn write_assembly<W: core::fmt::Write>(
        &self,
        insn: &[MceInstruction<Self::Instruction>],
        writer: &mut W,
    ) -> core::fmt::Result;

    /// Transforms a sequence of `insns` so that it is legal on the architecture, or emits an error if it cannot be legalized
    fn legalize_mce_context(
        &self,
        #[allow(unused_variables)] insns: &mut [MceInstruction<Self::Instruction>],
    ) -> std::io::Result<()> {
        Ok(())
    }

    /// Performs optional machine-code level optimizations on generated instruction sequences
    fn optimize_mce_context(
        &self,
        #[allow(unused_variables)] insns: &mut [MceInstruction<Self::Instruction>],
    ) {
    }
}

/// A Fully Built function (MCE)
pub struct BuiltMceFunction<I> {
    prologue: Vec<MceInstruction<I>>,
    basic_blocks: Vec<(String, Vec<MceInstruction<I>>)>,
}

impl<I> BuiltMceFunction<I> {
    /// Constructs a new [`BuiltMceFunction`] with the prologue body.
    pub fn new(prologue: Vec<MceInstruction<I>>) -> Self {
        Self {
            prologue,
            basic_blocks: Vec::new(),
        }
    }
    /// Pushes a new basic block with the given label and body
    pub fn push_basic_block(&mut self, label: String, insns: Vec<MceInstruction<I>>) {
        self.basic_blocks.push((label, insns))
    }

    /// Runs legalization passes on each independent MCE context (prologue and each basic block)
    pub fn legalize<M: MceWriter<Instruction = I>>(&mut self, mach: &M) -> std::io::Result<()> {
        mach.legalize_mce_context(&mut self.prologue)?;
        for (_, insns) in &mut self.basic_blocks {
            mach.legalize_mce_context(insns)?;
        }
        Ok(())
    }

    /// Runs optimization passes on each independent MCE context (prologue and each basic block)
    pub fn optimize<M: MceWriter<Instruction = I>>(&mut self, mach: &M) {
        mach.optimize_mce_context(&mut self.prologue);
        for (_, insns) in &mut self.basic_blocks {
            mach.optimize_mce_context(insns);
        }
    }

    /// Prints the assembly for the entire function
    pub fn print_assembly<M: MceWriter<Instruction = I>, W: core::fmt::Write>(
        &self,
        mach: &M,
        mut writer: W,
    ) -> core::fmt::Result {
        mach.write_assembly(&self.prologue, &mut writer)?;
        for (label, insns) in &self.basic_blocks {
            writer.write_str("\t")?;
            writer.write_str(label)?;
            writer.write_str(":\n")?;
            mach.write_assembly(insns, &mut writer)?;
        }
        Ok(())
    }

    /// Writes the macheine code for the entire function to the given [`InsnWrite`]
    pub fn write_machine_code<
        M: MceWriter<Instruction = I>,
        W: InsnWrite,
        F: FnMut(u128, String),
    >(
        &self,
        mach: &M,
        mut writer: W,
        mut sym_accepter: F,
    ) -> std::io::Result<()> {
        mach.write_machine_code(&self.prologue, &mut writer, &mut sym_accepter)?;
        for (label, insns) in &self.basic_blocks {
            sym_accepter(writer.offset() as u128, label.clone());
            mach.write_machine_code(insns, &mut writer, &mut sym_accepter)?;
        }
        Ok(())
    }
}
