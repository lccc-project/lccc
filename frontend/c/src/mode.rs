use std::collections::HashSet;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum LangOption {
    /// Whether or not `asm` is a keyword
    Asm,
    /// Whether or not `__asm__` is a keyword
    /// If [`LangOption::Asm`] is also set, controls whether `volatile` or `goto` can follow `asm`
    ///
    /// With `permissive` mode, treats `asm` as a keyword aliasing `__asm__`.
    AsmGcc,
    /// Whether msvc-style `__asm` is accepted.
    ///
    /// If [`LangOption::Asm`] is also set, controls whether `asm` allows msvc-style syntax (when followed by braces)
    AsmMsvc,
    /// Whether or not `bool` is a keyword
    /// If both this and [`LangOption::BoolC99`] are set, `bool` and `_Bool` are treated as the same token
    Bool,
    /// Whether or not `_Bool` is a keyword
    BoolC99,
    /// Whether or not `true` and `false are keywords`
    BoolLit,
    /// Whether or not `_Atomic` is a keyword
    /// If not present, the value of `stdc_no_atomics` is used
    AtomicC11,
    /// Whether or not `struct` and `union` types may be used in an `_Atomic` type
    AtomicStruct,
    /// Whether or not `_Complex` and `_Imaginary` are keywords
    ComplexC99,
    /// Whether or not `_Generic` is a keyword
    GenericC11,
    /// Whether or not `_Static_assert` is a keyword
    StaticAssertC11,
    /// Whether or not `static_assert` is a keyword
    StaticAssert,
    /// Whether or not C++11/C23 style [[attributes]] are supported
    Attribute,
    /// Whether or not GNU-style `__attribute__`s are accepted
    AttributeGcc,
    /// Whether or not MSVC-style `__declspec` attributes are accepted
    AttributeMsvc,
    /// Whether or not Capture-less closures are accepted
    ClosureEmpty,
    /// Whether or not capturing-closures are accepted
    ClosureCxx,
    /// Whether or not `*this` may be captured at the same time as `&`
    ClosureCaptureStarThisWithRef,
    /// Whether or not `constexpr` is a keyword
    Constexpr,
    /// Whether or not `constexpr` is accepted on variables
    ConstexprVariables,
    /// Whether or not `constexpr` is accepted on functions
    ConstexprFunctions,
    /// Whether or not inline is a keyword
    Inline,
    /// Whether or not inline has C99 inline behaviour
    InlineC99,
    /// Whether or not `__inline` is a keyword (and has C++/GNUC behaviour)
    InlineGcc,
    /// Whether or not restrict is a keyword
    Restrict,
    /// Whether or not `__restrict` is a keyword
    RestrictGcc,
    /// Whether or not clang block syntax is accepted
    BlockClang,
    /// Whether or not GNU block-expressions is accepted
    BlockExprGnu,
    /// Whether or not GNU labels as values and computed goto is accepted
    LabelRefGoto,
    /// Whether or not `char16_t` and `char32_t` are defined and are keywords
    UnicodeCharCxx,
    /// Whether or not `wchar_t` is a keyword
    WideCharCxx,
    /// Whether or not `char8_t` is defined and is a keyword
    Utf8CharCxx,
    /// Whether or not `nullptr` is a keyword
    NullptrCxx,
    /// Whether or not legacy 0 expressions (C/C++98) should be recognized as null pointer constants
    NullZeroC,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StdLangMode {
    /// Permissive mode controlled by `-std` only
    pub permissive: bool,
    /// value of `__STDC_VERSION__`
    pub stdc_version: Option<u32>,
    /// value of `__cplusplus`
    pub cpp_version: Option<u32>,
    /// Allowed syntax options
    pub lang_options: HashSet<LangOption>,
}
