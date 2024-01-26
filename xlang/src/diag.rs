use xlang_abi::{string::String, vec::Vec};

#[repr(u32)]
pub enum DiagnosticSeverity {
    Bug,
    Fatal,
    Error,
    Warning,
    Info,
    Note,
}

#[repr(C)]
pub struct DiagnosticText {
    pub severity: DiagnosticSeverity,
    pub text: String,
}

#[repr(C)]
pub struct DiagnosticElement {
    pub pretty_source_pos: String,
    pub display_source_begin_off: u64,
    pub display_source_end_off: u64,
    pub hl_source_begin_off: u64,
    pub hl_source_end_off: u64,
    pub text: Vec<DiagnosticText>,
}

#[repr(C)]
pub struct Diagnostic {
    pub pre_text: Vec<DiagnosticText>,
    pub elements: Vec<DiagnosticElement>,
}
