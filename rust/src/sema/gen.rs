use super::Mutability;

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum CaptureMode {
    MoveByVal,
    Ref(Mutability),
    CopyOfRef(Mutability),
}
