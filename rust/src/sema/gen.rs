use super::Mutability;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CaptureMode {
    MoveByVal,
    Ref(Mutability),
    CopyOfRef(Mutability),
}
