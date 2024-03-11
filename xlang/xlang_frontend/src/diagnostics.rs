use crate::span::{NoHygiene, Span};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct NoLocation;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct DiagPosition<R = NoLocation, H = NoHygiene> {
    pub span: Span<H>,
    pub extra: R,
}
