use validate::ValidationContext;
use validate::ValidationError;
use exec::ExecutionFrame;
use jit::{JitCompilerContext, JitError};

pub mod validate;
pub mod exec;
pub mod jit;

pub(crate) mod sealed{
    pub trait Sealed{}
}

pub trait Instruction: sealed::Sealed{
    fn id(&self)->u8;
    fn name(&self) -> &str;
    fn validate(&self,ctx: &mut ValidationContext)->Result<(),ValidationError>;
    fn eval(&self,frame: &mut ExecutionFrame);
    fn write_jit(&self,jit: &mut JitCompilerContext) -> Result<(),JitError>;
}

