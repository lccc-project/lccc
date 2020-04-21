
pub enum Register{
    Result,
}

pub enum Operand<'a>{
    Register(Register),
    LocalVariable(FrameLocalVariable<'a>),
    MemberOf(Box<Operand<'a>>,MemberField),
    AddressOf(Box<Operand<'a>>),
    Indirection(Box<Operand<'a>>),
    MemberOfIndirect(Box<Operand<'a>>,MemberField),
    Object(VariableReference)
}