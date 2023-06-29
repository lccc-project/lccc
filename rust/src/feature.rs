use crate::interning::Symbol;
use core::num::NonZeroU32;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum FeatureState {
    Stable(Since),
    Unstable(Option<TrackingIssue>),
    Experimental(Option<TrackingIssue>),
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Since {
    pub rust_version: Symbol,
    pub lccc_version: Option<Symbol>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct TrackingIssue {
    pub repo: Repo,
    pub issue_number: NonZeroU32,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Repo {
    Lccc,
    Rust,
    Custom(Symbol),
}

pub struct Features {}
