use crate::helpers::{CyclicOperationStatus, FetchIncrement};

use crate::parse;
use crate::span::Span;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct MacroDefId(u64);

impl core::fmt::Debug for MacroDefId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("%{}", self.0))
    }
}

impl core::fmt::Display for MacroDefId {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_fmt(format_args!("%{}", self.0))
    }
}

#[allow(dead_code)]
pub struct Macros {
    nextdefid: u64,
}

pub mod attr;
pub mod decl;
pub mod proc;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ErrorCode {
    IllFormedMacro,
    MacroCompilationError,
    BadExpansion,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Error {
    pub parse_err: Option<parse::Error>,
    pub text: Option<String>,
    pub span: Span,
    pub code: ErrorCode,
}

impl Error {
    pub fn from_parse_err(err: parse::Error, code: ErrorCode) -> Self {
        let span = err.span;
        Self {
            parse_err: Some(err),
            text: None,
            span,
            code,
        }
    }
}

pub type Result<T> = core::result::Result<T, Error>;
