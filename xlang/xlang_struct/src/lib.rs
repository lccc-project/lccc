#![deny(warnings, clippy::all, clippy::pedantic, clippy::nursery)]

//! Crate for structural representation of xlang ir
//! The types in this crate provide a high-level field of XIR that is easily programatically generatable, consummable, and manipulable.
//!
//! Each item in this section describes the syntax for that item in textual XIR using a modified ABNF. It is assumed that productions from other items are in-scope as non-terminals.
//! Additionaly, the following non-terminals are defined as unicode properties from Unicode 15.0.0. A character matches the non-terminal if it has that property:
//! * `XID_Start`
//! * `XID_Part`
//! * `White_Space`
//!
//! The ABNF Syntax used in this section is the same as specified by [`RFC 5234`](https://datatracker.ietf.org/doc/html/rfc5234), with the following exceptions:
//! * Non-terminals not enclosed in `<` brackets `>` are not used.
//! * Non-terminal names match the ABNF
//! ```abnf
//! non-terminal := <non-terminal-start> [*<non-terminal-char>]
//! non-terminal-start := %x41-%5A / %x61-7A / "_"
//! non-terminal-char := <non-terminal-start> / %x30-39 / "-"
//! ````
//!

use std::ops::{BitAnd, BitAndAssign, BitOr, BitOrAssign};

use xlang_abi::prelude::v1::*;

#[doc(hidden)]
pub mod macros;

pub mod fmt;

pub mod validate;

///
/// A component of a [`Path`], the name of a global object in xlang.
///
/// Path components can either be an identifier, some kind of special component which is an arbitrary string literal, or contain generics.
/// The first component of a [`Path`] may additionally be the root id.
///
/// The full ABNF for a path component in xlang is:
///
/// ```abnf
/// id-start := <XID_Start> / "_" / "$" / "."
/// id-part := <XID_Part> / "_" / "$" / "." / "#"
///
/// identifier :=  <id-start><id-part>*
///
/// special-path-component := "#" <string-literal>
///
/// generics := "<" [<generic-param> ["," *<generic-param>]] ">"
///
/// path-component := <identifier> / <special-path-component> / <generics>
/// ```
#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum PathComponent {
    /// The start of a [`Path`] that names objects in the global scope.
    /// This is allowed only as the first element of a [`Path`]
    /// This doesn't get parsed as a normal path component, but rather the absence of a path component preceeding a leading `::` in a path.
    Root,
    /// A normal Identifier component, such as `foo`, `bar`, or `_ZTV_ZNSt5alloc.ll_ZNSt5alloc15GlobalAllocatorE$_ZNSt5alloc6SystemE__`
    /// An xlang identifier matches the BNF:
    /// ```abnf
    /// id-start := <XID_Start> / "_" / "$" / "."
    /// id-part := <XID_Part> / "_" / "$" / "." / "#"
    ///
    /// identifier :=  <id-start><id-part>*
    /// ```
    Text(String),
    /// A special identifier component that otherwise isn't an identifier, such as `#"Foo"` or `#"signature:(int)->int"`.
    /// The contents of the String are largely unbounded
    ///
    /// A special path component matches the BNF:
    /// ```abnf
    /// special-path-component := "#" <string-literal>
    /// ``
    SpecialComponent(String),
    /// Parameters for instantiating a generic item, such as `<int(32)>` or `<float(32),{global_address foo}>`
    ///
    /// Generics match the BNF:
    /// ```abnf
    /// generics := "<" [<generic-parm> ["," *<generic-param>]] ">"
    /// ```
    Generics(Vec<GenericParameter>),
}

impl core::fmt::Display for PathComponent {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Root => Ok(()),
            Self::Text(st) => f.write_str(st),
            Self::SpecialComponent(comp) => f.write_fmt(format_args!("#{:?}", comp)),
            Self::Generics(generics) => {
                f.write_str("<")?;
                let mut sep = "";
                for generic in generics {
                    f.write_str(sep)?;
                    sep = ", ";
                    generic.fmt(f)?;
                }
                f.write_str(">")
            }
        }
    }
}

///
/// A [`Path`] in xlang is a named reference to a global object, such as `foo`, `::bar`, or `::baz::hi::<int(32),float(32),{const undef uinit *near segment(42) unique non_null addr_space(3) int(4096)}>`
///
/// A path matches the following BNF:
/// ```abnf
///
/// path := ["::"] <path-component> [*("::" <path-component>)]
///
/// ```
///
#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Path {
    /// The components of the path, separated by `::`
    pub components: Vec<PathComponent>,
}

impl core::fmt::Display for Path {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        let mut iter = self.components.iter();
        iter.next().unwrap_or(&PathComponent::Root).fmt(f)?;

        for i in iter {
            f.write_str("::")?;
            i.fmt(f)?;
        }
        Ok(())
    }
}

/// A compile-time parameter to a generic item (type, function, or static)
///
/// A generic parameter matches the BNF:
///
/// ```abnf
/// generic-arg := <type> / "{" <value> "}"
/// ```
#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum GenericParameter {
    /// A generic parameter type, such as `int(32)`
    Type(Type),
    /// A generic parameter type
    Value(Value),
}

impl core::fmt::Display for GenericParameter {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            GenericParameter::Type(ty) => ty.fmt(f),
            GenericParameter::Value(val) => {
                f.write_str("{")?;
                val.fmt(f)?;
                f.write_str("}")
            }
        }
    }
}

/// the content of an annotation
///
/// An Annotation Item matches the following BNF:
///
/// ```abnf
/// annotation-id := <path>
///
/// annotation-key-value := <annotation-id> "=" <annotation-value>
///
/// annotation-value := <annotation-id> / <string-literal> / <int-literal> / <value>
///
/// annotation-group-content := <annotation-value> / <annotation-key-value> / <annotation-group>
///
/// annotation-group := <annotation-id> "(" [<annotation-group-content> [*("," <annotation-group-content>)] ")"
///
/// annotation-item := <annotation-id> / <annotation-key-value> / <annotation-group>
///
///
/// ```
///
#[repr(u8)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AnnotationItem {
    /// A bare path in an annotation, such as `alignment` or `lccc::map`
    ///
    /// Matches the BNF:
    /// ```abnf
    /// annotation-id := <path>
    /// ```
    Identifier(Path),
    /// A key-value pair in an annotation, such as `debug_info_name = "foo"`
    ///
    /// Matches the BNF:
    /// ```abnf
    /// annotation-id := <path>
    /// ```
    Value(Path, Value),
    /// A named group in an annotation, such as `sort_fields(alignment)`
    ///
    /// Matches the BNF:
    /// ```anbf
    /// annotation-group := <annotation-id> "(" [<annotation-item> [*("," <annotation-item>)] ")"
    /// ``
    Meta(Path, Vec<Self>),
}

impl core::fmt::Display for AnnotationItem {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Identifier(id) => id.fmt(f),
            Self::Value(id, val) => f.write_fmt(format_args!("{} = {}", id, val)),
            Self::Meta(id, rest) => {
                id.fmt(f)?;
                f.write_str("(")?;
                let mut sep = "";
                for a in rest {
                    f.write_str(sep)?;
                    a.fmt(f)?;
                    sep = ", ";
                }
                f.write_str(")")
            }
        }
    }
}

/// An annotation, like `#[unwind_personality_routine(foo.__UW_personality)]`
///
/// Matches the following BNF:
/// ```abnf
/// annotation := "#" "[" <anotation-item> "]"
/// ```
///
#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Annotation {
    /// The content of the annotation
    pub inner: AnnotationItem,
}

impl core::fmt::Display for Annotation {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("#[")?;
        self.inner.fmt(f)?;
        f.write_str("]")
    }
}

/// Annotations stored by an annoted element, such as a struct body or declaration.
#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct AnnotatedElement {
    /// Annotations attached to the item
    pub annotations: Vec<Annotation>,
}

impl core::fmt::Display for AnnotatedElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for anno in &self.annotations {
            anno.fmt(f)?;
            f.write_str(" ")?;
        }
        Ok(())
    }
}

/// The visibility of a definition/declaration
///
/// Matches the BNF:
/// ```abnf
/// visibility := ["public" / "origin" / "module" / "private"]
/// ```
///
#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Visibility {
    /// Visible to all items (public)
    Public = 0,
    /// Visible to all items within the same top-level module (origin)
    Origin = 1,
    /// Visible to all items within the current module (module)
    Module = 2,
    /// Visible only to items within the current scope (private)
    Private = 3,
    /// Not visible to any item
    None = 4,
}

impl Default for Visibility {
    fn default() -> Self {
        Self::Public
    }
}

impl core::fmt::Display for Visibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Visibility::Public => f.write_str("public "),
            Visibility::Origin => f.write_str("origin "),
            Visibility::Module => f.write_str("module "),
            Visibility::Private => f.write_str("private "),
            Visibility::None => Ok(()),
        }
    }
}

///
/// The Member of a scope
///
/// This type matches the following ABNF:
///
/// ```abnf
/// scope-member := [*<annotation>] [<visibility>] <member-declaration>
/// ```
///
#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct ScopeMember {
    /// The annotations attached to the scope member.
    ///
    /// Matches `[*<annotation>]`
    pub annotations: AnnotatedElement,
    /// The Optional visibility of the scope member
    ///
    /// Matches `[<visibility>]`
    pub vis: Visibility,
    /// The actual declaration of the member.
    ///
    /// Matches `<member-declaration>`
    pub member_decl: MemberDeclaration,
}

/// The contents of a scope.
///
/// This type matches the ABNF:
/// ```abnf
/// scope := ["{" [<*annotation>] "}"] [*<scope-member>]
/// ```
#[repr(C)]
#[derive(Clone, Debug, Default)]
pub struct Scope {
    /// The annotations atteched to the scope, included in "{" and "}"
    ///
    /// Matches `[*<annotation>]`
    pub annotations: AnnotatedElement,
    /// The members of the scope, mapped by their name
    ///
    /// Matches `[*<scope-member>]`
    pub members: HashMap<Path, ScopeMember>,
}

/// A member declaration, excluding the name
///
/// Matches the non-terminal `member-declaration`.
///
/// The type does not contain the `<path>` non-terminal in each case
///
#[repr(u32)]
#[derive(Clone, Debug)]
pub enum MemberDeclaration {
    /// An empty [`MemberDeclaration`].
    ///
    /// Does not match any syntax
    Empty,
    /// A nested scope.
    ///
    /// Matches the abnf
    /// ```abnf
    /// member-declaration /= "scope" <path> "{" <scope> "}"
    /// ```
    Scope(Scope),
    /// A function declaration.
    ///
    /// Matches the abnf
    /// ```abnf
    /// member-declaration /= <function-declaration>
    /// ```
    Function(FunctionDeclaration),
    /// An opaque aggregate.
    ///
    /// Matches the abnf
    /// ```abnf
    /// member-declaration /= ("struct" / "union") <path> ";"
    /// ```
    OpaqueAggregate(AggregateKind),
    /// An aggregate with a body
    ///
    /// Matches the abnf
    /// ```abnf
    /// member-declaration /= ("struct" / "union") <path> [":" <base-class-spec>] <struct-body>
    /// member-declaration /= "enum" <path> ":" <primitive-type> (<enum-body> / ";")
    /// ```
    AggregateDefinition(AggregateDefinition),
    /// A static declaration or definition
    ///
    /// Matches the abnf
    /// ```abnf
    /// member-declaration /= [<linkage>] "static" <path> ":" <type> ["=" <value>] ";"
    /// ```
    Static(StaticDefinition),
}

/// The `Linkage` of a static or function declaration
///
/// Matches the abnf
/// ```abnf
/// linkage := "external" / "internal" / "const" / "weak"
/// ```
///
#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Linkage {
    /// The `"external"` linkage.
    /// Definitions with this linkage are accessible by the `qualified path` from all modules, and at most one definition with this linkage and the given `qualified path` may be defined accross all modules.
    External,
    /// The `"internal"` linkage.
    /// Definitions with this linkage are only accessible by the `qualified path` from the current module. Any number of such definitions may exist accross all modules,
    ///  and such definitions may coexist with definitions with `"external"` linkage
    Internal,
    /// The `"const"` linkage.
    /// Same as `"internal"`, but evaluating a `global_address` value that names the `function` or `static` is not guaranteed to yield a consistent address.
    ///
    /// Declarations with `"const"` linkage shall be a definition.
    Constant,
    /// The `"weak"` linkage.
    ///
    /// Same as `"external"` except that it has weak linkage behaviour.
    ///
    /// If the linkage is used on a declaration of a `static` or `function` that is not a definition,
    /// then a definition of the item is not required if you evaluate a `global_address` that names the `static` or `function`, and a definition is not provided, the result is a null pointer of the type of the `global_address`.
    ///
    /// If the linkage is used on a definition of a `static` or `function`, then it has weak definition behaviour.
    /// If the definition has a `#[comdat]` annotation, then the definition obeys the behaviour of the `#[comdat]` annotation.
    ///
    /// Otherwise, a program may define any number of definitions with `"weak"` linkage with the same `qualified` path accross all modules., and up to one definitions with `"external"` linkage.
    /// If one definition with `"external"` linkage is provided, then any evaluation of a `global_address` value naming the `static` or `function` will evaluate to the address of that one definition.
    /// Otherwise, any evaluation of a `global_address` value naming the `static` or `function` will evaluate to the address of a single, unspecified, definition from among those definitions,
    /// Which definition is chosen is consistent accross all `global_address` value evaluations accross all values.
    ///
    /// A `function` with `"weak"` linkage cannot be inlined, unless it has an `#[inline]` annotation. A `function` with `"weak"` linkage cannot be called from a constant evaluation.
    ///
    /// ### The `#[comdat]` annotation
    ///
    /// Annotation syntax:
    /// ```abnf
    /// annotation /= "#" "[" <comdat-specification> "]"
    ///
    /// comdat-specification := "comdat" "(" <comdat-group-specifier> ")"
    ///
    /// comdat-group-specifier := <annotation-id> / (<comdat-specifier> [*("," <comdat-specifier>)])
    ///
    /// comdat-specifier := ("group" "=" <annotation-id>) / ("mode" "=" <comdat-mode>) / ("priority" "=" <int-literal>) / <annotation-key-value>
    /// ```
    ///
    /// Allows specifying a `comdat` group for a function with `"weak"` linkage.
    /// Two forms are used, one form is specified by putting the group name directly inside the comdat-specification.  
    /// The second form is an elaborated form. allowing additional options to be specified. Note that only the `group` option in this form is guaranteed to be respected.
    ///
    /// The `comdat` annotation may not be applied to a function or static declaration that is not a definition, or to a function or static declaration with linkage other than `"weak"` or `"const"`.
    ///
    /// The `group` specifies the name of the comdat group the item belongs to. Multiple definitions with `"weak"` linkage may be placed in a given comdat group.
    /// The evaluation of any `global_address` referring to any `static` or `function` that belongs to the same comdat group, accross all modules, shall refer to the definition from the same module as
    ///  every evaluation of any other `global_address` which refers to a `static` or `function` in the same comdat group.
    ///
    /// The `mode` specifies the behaviour of the linking step of all modules. The default mode is `linkonce`. Additional options are defined:
    /// * `validate`: Hints that the linker should diagnose the constraints specified in this section (Implementation note: This will typically only operate when using Link Time Code Generation)
    ///
    /// A specified `mode` is not guaranteed to have any effect beyond the default behaviour.
    ///
    /// The `priority` hints to the linker which module's definition should be used, according to the priority specification.
    /// A higher priority indicates the linker should prefer to select the definitions from that module rather than modules with lower priority.
    /// If `priority` differs between definitions in the same module and comdat group, which priority is used is unspecified.
    /// The default `priority` is 0. It is not guaranteed that a specified `priority` above 0 will have any effect.
    ///
    /// A `"const"` item in a `#[comdat]` group allows `"const"` items to be deleted if the definitions from the module it is defined in are not chosen. It has no other effect when applied to a function or static with `"const"` linkage.
    ///
    ///
    /// The program is ill-formed if any of the following constraints are violated. No diagnostic is required:
    /// * The program refers to a `"const"` item with a `#[comdat]` annotation in a `global_address` value that is not one of the following:
    ///     * The initializer of a `static` with a `#[comdat]` annotation specifying the same `#[comdat]`
    ///     * As a `const` expression  evaluated in a `function` definition that is declared specifying the same `#[comdat]`
    /// * The program defines a `function` or a `static` in one module with `"weak"` linkage and a given `qualified path` and a `#[comdat]` annotation,
    ///  and the same `qualified path` is not defined in the same `#[comdat]` group in every module that a `function` or a `static` with `"weak"` linkage in the same `#[comdat]` group
    /// * The program defines a `function` or a `static` in one module with `"weak"` linkage and a given `qualified path` and a `#[comdat]` annotation,
    ///  and defines a `function` or a `static` with the same `qualified path` with `"external"` linkage, with `"weak"` linkage without a `#[comdat]` annotation, or with `"weak"` linkage in a different `#[comdat]` group.
    ///  
    ///
    Weak,
}

impl Default for Linkage {
    fn default() -> Self {
        Linkage::External
    }
}

impl core::fmt::Display for Linkage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::External => f.write_str("external"),
            Self::Internal => f.write_str("internal"),
            Self::Weak => f.write_str("weak"),
            Self::Constant => f.write_str("const"),
        }
    }
}

bitflags::bitflags! {
    /// Specifiers for a static declaration or definition
    ///
    /// Matches the abnf
    /// ```abnf
    /// static-specifier := "immut"
    /// ```
    #[repr(transparent)]
    pub struct StaticSpecifier : u16{
        const IMMUTABLE = 0x001;
    }
}

impl core::fmt::Display for StaticSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.contains(Self::IMMUTABLE) {
            f.write_str("immut ")?;
        }
        Ok(())
    }
}

/// A `static` (Value) definition
///
/// Matches the abnf
/// ```abnf
/// member-declaration /= [<linkage>] "static" [*<static-specifier>] <path> ":" <type> ["=" <value>] ";"
/// ```
#[repr(C)]
#[derive(Clone, Debug)]
pub struct StaticDefinition {
    /// The type of the static initializer
    /// Matches `<type>`
    pub ty: Type,
    /// The initializer or [`Value::Empty`] if the declaration is not a declaration
    /// Matches `["=" <value>]`
    pub init: Value,
    /// The linkage of the `static` declaration.
    ///
    /// Matches `[<linkage>]`
    pub linkage: Linkage,
    /// The specifiers for the `static` declaration
    ///
    /// Matches `<static-specifier>`
    pub specifiers: StaticSpecifier,
}

impl Default for MemberDeclaration {
    fn default() -> Self {
        Self::Empty
    }
}

bitflags::bitflags! {
    /// Validity flags for scalar types.
    ///
    /// Matches the abnf
    ///
    /// ```abnf
    /// scalar-validity := [*("nonzero" / "finite" / "nonnan")]
    /// ```
    ///
    /// Scalar validity flags are enforced on load and store, or on computation.
    ///
    /// If a value that violates a scalar validity constraint is loaded or stored, the value loaded/stored becomes `invalid` of the type.
    /// If a computation (including a conversion) operation yields a value that violates s scalar validity constraint, the result is `uninit` of the type.
    ///
    /// `uninit` is considered invalid for any scalar type that has any validity constraint - loading/storing such a value yields `invalid`.
    ///
    #[repr(transparent)]
    #[derive(Default)]
    pub struct ScalarValidity : u8{
        /// The `scalar-validity` specifier `"nonzero"`.
        /// Indicates that a zero value is not permitted.
        const NONZERO = 0x01;
        /// The `scalar-validity` specifier `"finite"`.
        ///
        /// Indicates that infinities and `NaR` values are not permitted.
        ///
        /// Only effective when applied to a `float`, `posit`, or `fract` type.
        const FINITE = 0x02;
        /// The `scalar-validity` specifier `"nonnan"`.
        ///
        /// Indicates that the float `NaN` values are not permitted.
        ///
        /// Only effective when applied to a `float` type.
        const NONNAN = 0x04;
    }
}

/// The header (common portion) of a scalar type.
/// Matches
/// ```abnf
/// scalar-header := <scalar-validity> ["vectorsize"(<integer>)](<integer>)
/// ```
#[repr(C)]
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ScalarTypeHeader {
    /// The size (in bits) of the scalar type. In the case of a vector, this is the width of each lane
    pub bitsize: u16,
    ///The number of lanes of a vector type, if specified, or `None`
    pub vectorsize: Option<u16>,
    /// The validity specifier of the `scalar-type`
    pub validity: ScalarValidity,
}

bitflags::bitflags! {
    /// Specifiers for a `{s,u}char` type
    ///
    /// Matches the abnf
    /// ```abnf
    /// char-specifier := ["unicode"]
    /// ```
    #[repr(transparent)]
    pub struct CharFlags: u8{
        /// Whether or not the type is considered to be signed or unsigned
        ///
        /// Determined by the name of the type `schar` vs. `uchar`
        const SIGNED = 1;
        /// Whether or not the type is restricted to unicode only (`char8_t`, `char16_t`, `char32_t`).
        ///
        /// Matches `"unicode"`
        const UNICODE = 2;
    }
}

/// The format of a floating-point type
///
/// Matches the ABNF
/// ```abnf
/// float-format := ["binary" / "decimal" / "dfloat" / "extrange" / "extprecision"]
/// ```
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum FloatFormat {
    /// The IEEE-754 binary format, in the specified width.
    ///
    /// If the width is 16, 32, 64, 128, or 128+32k, then the format exactly matches IEEE-754 for the given binary interchange format.
    ///
    /// Otherwise, if the width is greater than `8`, then corresponds to the extended binary format with the exponent width, exponent max, and exponent bias of the next highest standard format,
    ///  and a number of significand bits corresponding to the remaining size of the value.
    ///
    /// If `width` is 8 or less, then an exponent width is 3 and the exponent bias is -7. IEEE Binary formats with width < 3 is not supported.
    ///
    /// This matches the specifier `binary` and is the default if not `float-format` is specified.
    ///
    IeeeBinary,
    /// The IEEE-754 decimal format, in the specified width, with the significand encoded in BCD.
    ///
    /// If the width is a multiple of 32, then the format is as specified by IEEE-754 for the given decimal interchange format.
    /// Otherwise, if the width is greater than `8`, then corresponds to the extended decimal format with the exponent width, exponent max, and exponent bias of the next highest standard format,
    ///  and a number of significand bits corresponding to the remaining size of the value.
    ///
    /// IEEE 754 Decimal formats with width <= 8 are not supported.
    ///
    /// This matches the specifier `decimal`.
    ///
    IeeeDecimal,
    /// The IBM Double-Double format (sum of doubles), modified to support any width of floating-point type.
    ///
    /// The value contains 2 `binary` values packed together, both with width given by `width/2`. If `width` is odd, then the first component will use this bit.
    ///
    /// Regardless of with, there is no padding bits between either format. The first component is considered most significant, and the second component is least significant.
    /// The Most significant component is always larger than the least significant component unless they are both `NaN`.
    ///
    /// This matches the specifier `dfloat`
    IbmDfloat,
    /// An IEEE Type with extended range, but not extended precision.
    ///
    /// If `width` is not 16, 32, 64, or 128, this exactly matches the `binary` format.
    ///
    /// If `width` is 16, 32, or 64, then it matches the IEEE Extended Binary Format with the given total width, but the same exponent width, exponent max, and exponent bias as the next larger binary interchange format.
    ///
    /// If `width` is 128, then this matches the IEEE Extended Binary Format with the total width 128, but with an exponent width, exponent max, and exponent bias, of 19, 262143, and -262143. (This corresponds to the exponent width, max, and bias of the binary256 format).
    ///
    /// This appropriately encodeds the `bfloat16` format when used with a width of 16
    ///
    /// This matches the specifier `extrange`
    IeeeExtRange,
    /// An IEEE Type with extended precision, but not extended range.
    ///
    /// If `width` is 8, 16, 32, 64, 128, or 128+32k, then this exactly matches the `binary` format.
    ///
    /// If `width` is greater than 16, this matches the IEEE Extended Binary Format with the given total width, but with an exponent width, exponent max, and exponent bias given by the next lowest binary interchange format.
    ///
    /// Otherwise, this matches the IEEE Extended Binary Format with the given total width, but with exponent width, exponent max, and exponent bias of 3, 3, and -3 respectively.
    ///
    /// This appropriately encodes the x87 double-extended precision when used with a width of 80.
    ///
    /// This matches the specifier `extprecision`
    IeeeExtPrecision,
}

/// The kind of a scalar type, and kind-specific information
///
/// Matches the ABNF non-terminal `scalar-type-kind`
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
#[repr(u8)]
pub enum ScalarTypeKind {
    /// Default/invalid scalar type kind.
    /// Does not match any syntax
    Empty,
    /// Signed or Unsigned integer types
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// scalar-type-kind /= ("int" / "uint") [*(<int-range-specifier>)]
    /// int-range-specifier := ("min" / "max") "(" ["-"] <int-literal> ")"
    /// ```
    Integer {
        /// Whether or not the integer type is signed.
        /// The leading keyword `int` is a signed and the keyword `uint` is unsigned.
        signed: bool,
        /// The minimum value (as a value of the type base integer type with the same size) the type can have, if any.
        /// This specifier operates in the same manner as the scalar validity specifiers
        min: Option<i128>,
        /// The maximum value (as a value of the type base integer type with the same size) the type can have, if any.
        /// This specifier operates in the same manner as the scalar validity specifiers
        max: Option<i128>,
    },
    /// A floating-point type with the specified format
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// scalar-type-kind /= "float" [<float-format>]
    /// ```
    Float {
        /// The format of the floating-point type
        /// Matches `<float-format>`
        format: FloatFormat,
    },
    /// A Posit Real Number type with the specified format.
    /// The `es` value is 2 regardless of width. A posit type with width of 4 bits or less is not supported
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// scalar-type-kind /= "posit"
    /// ```
    Posit,
    /// A fixed-point number. with a specified number of fractional bits.
    ///  
    /// Matches the ABNF syntax
    /// ```abnf
    /// scalar-type-kind /= "fixed" "fractbits" "(" <int-literal> ")"
    /// ```
    Fixed {
        /// Specifies the number of fractional bits in the fixed point value (with the remaining most significant bits being)
        ///
        /// Specifying a larger value than `width` in a fixed point type is not supported.
        fractbits: u16,
    } = 4,
    /// A character type
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// scalar-type-kind := ("schar" / "uchar") <char-specifier>
    /// ```
    Char {
        /// `flags` specifying the `char` type.
        /// the `CharFlags::SIGNED` flag is set based on `"schar` vs. `"uchar"`. The remaining flags match `<char-specifier>`
        flags: CharFlags,
    } = 5,
}

impl Default for ScalarTypeKind {
    fn default() -> Self {
        Self::Empty
    }
}

/// A scalar type
///
/// Matches the ABNF syntax
/// ```abnf
/// scalar-type := <scalar-type-kind> <scalar-type-header>
/// ````
#[repr(C)]
#[derive(Copy, Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ScalarType {
    /// The header (size, and validity) of the scalar-type.
    /// Matches `<scalar-type-header>`
    pub header: ScalarTypeHeader,
    /// The kind of the scalar-type.
    /// Matches `<scalar-type-kind>`
    pub kind: ScalarTypeKind,
}

impl core::fmt::Display for ScalarType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self.kind {
            ScalarTypeKind::Empty => panic!(),
            ScalarTypeKind::Integer { signed, min, max } => {
                if !signed {
                    f.write_str("u")?;
                }

                f.write_str("int")?;

                if let Some(min) = min {
                    f.write_str(" min")?;
                    f.write_str("(")?;
                    min.fmt(f)?;
                    f.write_str(")")?;
                }
                if let Some(max) = max {
                    f.write_str(" max")?;
                    f.write_str("(")?;
                    max.fmt(f)?;
                    f.write_str(")")?;
                }
            }
            ScalarTypeKind::Float { format } => {
                f.write_str("float")?;
                match format {
                    FloatFormat::IeeeBinary => {}
                    FloatFormat::IeeeDecimal => f.write_str(" decimal")?,
                    FloatFormat::IbmDfloat => f.write_str(" dfloat")?,
                    FloatFormat::IeeeExtRange => f.write_str(" extrange")?,
                    FloatFormat::IeeeExtPrecision => f.write_str(" extprecision")?,
                }
            }
            ScalarTypeKind::Posit => f.write_str("posit")?,
            ScalarTypeKind::Fixed { fractbits } => {
                f.write_str("fixed fractbits(")?;
                fractbits.fmt(f)?;
                f.write_str(")")?;
            }
            ScalarTypeKind::Char { flags } => {
                if flags.contains(CharFlags::SIGNED) {
                    f.write_str("s")?;
                }
                f.write_str("char")?;

                if flags.contains(CharFlags::UNICODE) {
                    f.write_str(" utf")?;
                }
            }
        }

        if self.header.validity.contains(ScalarValidity::NONZERO) {
            f.write_str(" nonzero")?;
        }

        if self.header.validity.contains(ScalarValidity::FINITE) {
            f.write_str(" finite")?;
        } else if self.header.validity.contains(ScalarValidity::NONNAN) {
            f.write_str(" nonnan")?;
        }

        if let Some(vectorsize) = self.header.vectorsize {
            f.write_str(" vectorsize")?;
            f.write_str("(")?;
            vectorsize.fmt(f)?;
            f.write_str(")")?;
        }

        f.write_str("(")?;
        self.header.bitsize.fmt(f)?;
        f.write_str(")")
    }
}

impl TryFrom<Type> for ScalarType {
    type Error = ();
    fn try_from(other: Type) -> std::result::Result<Self, ()> {
        if let Type::Scalar(sty) = other {
            Ok(sty)
        } else {
            Err(())
        }
    }
}

/// A type used for function signatures
///
/// Matches the ABNF non-terminal `type`
///
/// ### Classes of Types
///
/// Certain types are considered "Value Types". These are types which a value on the operand stack can take. The value types are:
/// * Scalar Types
/// * Array types
/// * Pointer types (regardless of whether or not the pointee type is a value type)
/// * A tagged or aligned type, if the base type is a value type
/// * A Product type
/// * An inline Aggregate type
/// * A Named type that refers to an aggregate definition or a type alias of a value type
///
/// The complete value types are all value types for which the size of the value is known. An rvalue operand must be of a complete value type (lvalue operands can be any value type).
/// The complete value types are all value types except:
/// * An array of an unknown bound
/// * A Named type that refers to an opaque aggregate declaration.
///
/// A static declaration must have a value type. Such a static may only have an initializer if the declaration has a complete value type.
/// Each parameter of a function type or a function declaration must have a complete value type, and the return value of a function must either be `void()` or a complete value type.
///
/// ### Type Compatibility
///
/// There is an equivalence relation, known as "Compatibility", between two types. Types must be compatible in order to be ABI equivalent for function calls or for static declarations/definitions.
/// Compatible types also have the same object and value representations, and the same size and alignment requirement.
///
/// Two types, `T` and `U` are compatible if:
/// * They are the same type
/// * `T` is a scalar type, and `U` is a scalar type of the same kind, width, and vector size
/// * `T` is a signed or unsigned integer or character type and `U` is a signed or unsigned integer or character type of the same width and vector size
/// * `T` and `U` are both pointers with pointee types `S` and `R` respectively, `T` and `U` both have the same pointer width specifier, `S` and `R` are either both a value type or `void()`, or are both function types
/// * `T` and `U` are both function types with the same number of parameters, and:
///     * They both have the same tag,
///     * Each parameter pairwise is compatible,
///     * Either both types are varargs or neither are.
/// * `T` and `U` are both aligned types with the same alignment value, and the underlying types are compatible
/// * `T` is an aligned type with underlying type `U`, and the alignment requirement of `U` is at least the specified alignment value
/// * `T` is a tagged type with underlying type `U`,
/// * `T` and `U` are both product types, with the same number of elements, and each element of both types are of compatible types,
/// * `T` and `U` are both inline aggregate types, and:
///     * Both are of the same aggregate kind,
///     * In the case of an `enum`, both have the same underlying type,
///     * In the case of a `struct`, both are standard-layout, have the same number of fields, and for each pair of fields in order from both types:
///         * Both fields are of compatible types, and
///         * Neither field is a bitfield, or
///         * Both fields are bitfields of the same length
///     * In the case of a `union`, each field of one `union` has a corresponding field in the other `union`, such that:
///         * Both fields are of compatible types, and
///         * Neither field is a bitfield, or
///         * Both fields are bitfields of the same length
/// * `T` is a named type referring to an aggregate definition and `U` is an inline aggregate type, and:
///     * Both are of the same aggregate kind,
///     * In the case of an `enum`, both have the same underlying type,
///     * In the case of a `struct`, both are standard-layout, have the same number of fields, and for each pair of fields in order from both types:
///         * Both fields are of compatible types, and
///         * Neither field is a bitfield, or
///         * Both fields are bitfields of the same length
///     * In the case of a `union`, each field of one `union` has a corresponding field in the other `union`, such that:
///         * Both fields are of compatible types, and
///         * Neither field is a bitfield, or
///         * Both fields are bitfields of the same length
/// * `T` and `U` are both array types, either both have the same bound or at least one has an unknown bound, and the element types are compatible,
/// * `T` is an `enum` type and `U` is its underlying type.
#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Type {
    /// An empty/invalid type.
    /// Does not match any syntax
    Null,
    /// A Scalar Type, such as `int(32)` or `float(64)`.
    ///
    /// Scalar types are complete value types
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= <scalar-type>
    /// ```
    Scalar(ScalarType),
    /// The `void()` type, which can indicate a function with no return value or a pointer with no specific pointee
    ///
    /// The `void()` type is not a value type, but may appear as the return type of a function type.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= "void" "(" ")"
    /// ```
    Void,
    /// A function type, used behind a pointer to refer to a callable function.
    ///
    /// No function type is a value type.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= "function" <signature>
    /// ```
    ///
    FnType(Box<FnType>),
    /// A pointer type, which points to an object, label, or function
    /// All pointer types are complete value types.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= <pointer-type>
    /// ```
    Pointer(PointerType),
    /// An array type, including an array of unknown bound.
    /// All array types are value types, and an array with known bound is a complete value type.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= <array-type>
    /// ```
    Array(Box<ArrayType>),
    /// A tagged type, which can statically distinguish between two otherwise equivalent types.
    /// A tagged type has an underlying type, which the tagged type inherits the properties from.
    ///
    /// A tagged type is a value type if the underlying type is a value type, and is complete if the underlying type is.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= "tagged" "(" <int-literal> ")" <type>
    /// ````
    ///
    TaggedType(u16, Box<Self>),
    /// An inline product type, which stores 0 or more values of known types, called element types.
    /// Each element type must be a complete value type. A product type is a complete value type.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= "product" "(" [<type> [*("," <type>)]] ")"
    /// ````
    Product(Vec<Self>),
    /// An aligned type, which stores a value with a greater alignment requirement.
    /// An aligned type has an underlying type, which must be a value type.
    /// An aligned type is a value type, and if the underlying type is complete, then the aligned type is complete as well.
    ///
    /// An aligned type has a given alignment as a value. The value must be of an integer type, and must neither be `uninit` or `invalid`. The value must be a power of two less than a target-dependent maximum.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= "aligned" "(" <value> ")" <type>
    /// ````
    ///
    Aligned(Box<Value>, Box<Self>),
    /// An inline aggregate type.
    ///
    /// Inline aggregates allow for aggregate types to be declared immediately at any place the aggregate type is used.
    /// Each equal aggregate type is the same, regardless of its declaration location. Inline aggregates cannot have base classes (but inline enum types can have an underlying type)
    ///
    /// Inline aggregate types are always complete value types.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= <aggregate-type> <aggregate-body>
    /// type /= "enum" [":" <scalar-type>] <enum-body>
    /// ```
    ///
    Aggregate(AggregateDefinition),
    /// A named type that refers to either an aggregate definition, opaque aggregate, or a type alias.
    /// If a named type refers to a type alias, then it is the same type as the aliased type.
    ///
    /// Otherwise, it refers to a type that is unique per `qualified path`, which is a value type.
    /// If it refers to an aggregate definition in the current module, then it is a complete value type.
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// type /= <path>
    /// ```
    Named(Path),
}

impl core::fmt::Display for Type {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Null => Ok(()),
            Self::Scalar(st) => st.fmt(f),
            Self::Void => f.write_str("void()"),
            Self::TaggedType(tag, ty) => {
                f.write_str("tagged(")?;
                tag.fmt(f)?;
                f.write_str(") ")?;
                ty.fmt(f)
            }
            Self::Product(tys) => {
                f.write_str("product(")?;
                let mut iter = tys.iter();
                if let core::option::Option::Some(first) = iter.next() {
                    first.fmt(f)?;
                }
                for ty in iter {
                    f.write_str(", ")?;
                    ty.fmt(f)?;
                }

                f.write_str(")")
            }
            Self::FnType(fnty) => {
                f.write_str("function")?;
                fnty.fmt(f)
            }
            Self::Pointer(pty) => pty.fmt(f),
            Self::Array(arrty) => {
                f.write_str("[")?;
                arrty.ty.fmt(f)?;
                f.write_str(";")?;
                arrty.len.fmt(f)?;
                f.write_str("]")
            }
            Self::Named(name) => f.write_fmt(format_args!("({})", name)),
            Self::Aggregate(defn) => defn.fmt(f),
            Self::Aligned(alignment, base) => {
                f.write_fmt(format_args!("aligned({}) {}", alignment, base))
            }
        }
    }
}

/// The kind of an aggregate definition
/// Matches
/// ```abnf
/// aggregate-kind := "struct" / "union"
/// ```
#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum AggregateKind {
    /// A `struct`. Each field is layed out in non-overlapping storage
    ///
    /// ### The `#[layout]` annotation
    ///
    /// A struct definition or a `struct` inline aggregate may have the `#[layout]` annotation, defined herein. This controls the order fields are layed out.
    ///
    /// ```abnf
    /// annotation := "#" "[" <layout-def> "]"
    /// layout-def := "layout" "(" [<layout-specifier> [*("," <layout-specifier>)]] ")"
    /// layout-specifier := ("sort_fields" "(" <sort-quality-specifier> ["," <sort-order-specifier>] ")") / <layout-alignment-specifier>
    /// sort-quality-specifier := "alignment" / "size"
    /// sort-order-specifier := "ascending" / "descending"
    /// layout-alignment-specifier := (("align" / "packed") "(" <annotation-value> ")") / "packed"
    /// ```
    ///
    /// The sort layout-specifier allows fields to be reordered according to either alignment or size to achieve the minimum possible layout. At most one `sort_fields` layout-specifier may appear.
    /// Groups of consecutive bitfieilds (separated by either a non-bitfield field or a length 0 bitfield) are reordered as a group, and use the total size or maximum alignment in the determination.
    ///
    /// The `layout-alignment-specifier` may appear to increase or decrease the struct's alignment requirement (and, in the case of `packed`, interior padding).
    /// At most one `layout-alignment-specifier` may appear
    ///
    Struct,
    /// A `union`. Each field is layed out in the same storage, starting at offset `0`.
    Union,
}

bitflags::bitflags! {
    /// The specifiers attached to a aggregate field
    ///
    /// Matches the ABNF syntax
    /// ```abnf
    /// field-specifier := "mutable"
    /// ```
    #[repr(transparent)]
    pub struct AggregateFieldSpecifier : u32{
        /// Indicates that the field is writable, regardless of `readonly`, `readshallow`, or top level `immutable binding`
        const MUTABLE = 0x0001;
    }
}

impl core::fmt::Display for AggregateFieldSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.contains(Self::MUTABLE) {
            f.write_str("mutable ")?;
        }
        Ok(())
    }
}

/// The definition of an aggregate field.
///
/// Matches the ABNF syntax
/// ```abnf
/// aggregate-field := <annotated-element> [*<field-specifier>] <ident> ":" <type> [":" <value>]
/// ```
#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AggregateField {
    /// The annotations on the field
    /// Matches `<annotated-element>`
    pub annotations: AnnotatedElement,
    /// The flags of a field.
    /// Matches `<field-specifier>`
    pub specifiers: AggregateFieldSpecifier,
    ///The name of the field
    /// Matches `<ident>`
    pub name: String,
    /// The type of the field
    ///
    /// Matches `<type>`
    pub ty: Type,
    /// Matches `<value>` if present
    pub bitfield_width: Value,
}

impl core::fmt::Display for AggregateField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.annotations.fmt(f)?;
        self.specifiers.fmt(f)?;
        self.name.fmt(f)?;
        f.write_str(": ")?;
        self.ty.fmt(f)?;
        if self.bitfield_width != Value::Empty {
            f.write_str(": ")?;
            self.bitfield_width.fmt(f)?;
        }
        Ok(())
    }
}

/// The definition of an aggregate type.
///
/// Matches the ABNF syntax
/// ```abnf
/// aggregate-definition := <annotated-element> "{" [<aggregate-field> [*("," <aggregate-field>)] "}"
/// ```
#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AggregateDefinition {
    /// The annotations of the aggregate body. Matches the `<annotated-element>` portion of the aggregate-definition non-terminal
    pub annotations: AnnotatedElement,
    /// The kind of the aggregate. Not part of the syntax
    pub kind: AggregateKind,
    /// The fields of the aggregate.
    /// Matches each `<aggregate-field>`
    pub fields: Vec<AggregateField>,
}

impl core::fmt::Display for AggregateDefinition {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self.kind {
            AggregateKind::Struct => f.write_str("struct")?,
            AggregateKind::Union => f.write_str("union")?,
        }

        for a in &self.annotations.annotations {
            f.write_str(" ")?;
            a.fmt(f)?;
        }

        f.write_str("{")?;
        let mut sep = " ";
        for field in &self.fields {
            f.write_str(sep)?;
            sep = ", ";
            field.fmt(f)?;
        }
        f.write_str(" }")
    }
}

impl Default for Type {
    fn default() -> Self {
        Self::Null
    }
}

/// An Array type.
///
/// Matches
///
#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct ArrayType {
    /// The type of the array
    pub ty: Type,
    /// The length of the array
    pub len: Value,
}

bitflags::bitflags! {
    /// An aliasing Rule for a pointer, that indicates what assumptions are made about the pointer.
    ///
    /// All valid pointers exist in at least one of two modes for any given byte:
    /// * `Valid`
    /// * `Disabled`
    ///
    /// A pointer that is marked as valid for any given byte may freely read from and (subject to restrictions) freely write to that byte.
    /// When a pointer is `Disabled` for a byte, writing to that byte via that pointer and reads yield uninit scalar values.
    ///
    /// The determination of whether a particular access is valid occurs after the effect of any aliasing attribute is applied.
    /// A pointer that is marked as `Disabled` for a byte cannot be marked in any other way - any operation that attempts to do so is ignored.
    ///
    /// When a pointer is derived, the Valid/Disabled state of each byte is copied to the child.
    ///
    /// Matches the syntax
    /// ```abnf
    /// pointer-aliasing = "unique" / "readonly" / "nonnull"
    /// ```
    #[repr(transparent)]
    #[derive(Default)]
    pub struct PointerAliasingRule : u32{
        /// Indicates that no other pointers access any of the same bytes while this pointer is active.
        ///
        /// Matches `"unique"`
        ///
        /// ## Activation
        /// For any given byte accessible through a `unique` pointer which is `Valid` for that byte, the pointer has two modes.
        /// * Active
        /// * Frozen
        /// * Reserved
        ///
        /// When a pointer is derived as `unique` the pointer is marked as `Reserved` in each byte accessible from that pointer.
        ///
        /// Regardless of mode, the pointer may make any read access. In `Reserved` or `Active` mode, write accesses may be performed.
        ///
        /// When a pointer is used to write to a byte, then the following happens:
        /// * That pointer (if `unique`) and all of its ancestors marked `unique` are set to `Active` for that byte. If any of these pointers were marked `Frozen` for that byte, instead that pointer and all descendants are marked `Disabled`.
        /// * All pointers that are not an ancestor of the pointer are marked as `Disabled` for that byte.
        /// * All `readonly` pointers and all children of readonly pointers are marked as `Disabled` for that byte.
        ///
        /// When a read access occurs to byte through any pointer, the following happens:
        /// * All pointers that are `unique` and not an ancestor that are `Reserved` for that byte become `Frozen`.
        /// * All pointers that are `unique` and are `Active` for that byte and not an ancestor become `Disabled`.
        ///
        ///
        /// `unique` may be combined with `readonly`, but has no effect as `readonly` prohibits write accesses and `unique` activation requires a write access.
        const UNIQUE = 2;
        /// Indicates that no pointer may perform a write access to any of the same bytes while this pointer is active.
        ///
        /// A `readonly` pointer becomes `Disabled` for a byte when any pointer (included a descendant) is used to perform a write access to that byte.
        const READ_ONLY = 4;

        /// Indicates that the pointer may not be null.
        /// This is a trivial validity attribute and does not impose any additional aliasing constraints.
        const NONNULL = 32;
    }
}

bitflags::bitflags! {
    /// Determines the type of valid range.
    ///
    /// A byte is accessible from a pointer if:
    /// * The pointer is a root pointer to an object, the byte belongs to the complete object the root pointer points to,
    /// * If the pointer was returned from the intrinsic function `__lccc::xlang::limit_valid_range`, the byte is part of the range `[p, p+n)` where the parameters to the intrinsic call were `[.., p, n]`, or
    /// * If the pointer was not returned from the intrinsic function `__lccc::xlang::limit_valid_range` and the pointer is a child of some pointer `p`, the byte is accessible from `p`.
    ///
    /// A range of bytes is accessible from a pointer if each byte contained in the range is accessible from that pointer and each byte in the range belongs to the same complete object.
    ///
    /// Syntax:
    /// ```abnf
    /// valid-range-option := "inbounds" / "read" / "write"
    /// ```
    ///
    #[repr(transparent)]
    #[derive(Default)]
    pub struct ValidRangeType : u16{
        /// Indicates that the valid range of the pointer
        const INBOUNDS = 1;
        const READ = 2;
        const WRITE = 4;
    }
}
bitflags::bitflags! {
    #[repr(transparent)]
    #[derive(Default)]
    pub struct PointerDeclarationType : u16{
        const REF = 1;
        const CONST = 2;
        const VOLATILE = 4;
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Default, Hash)]
    pub enum struct PointerKind{
        Default = 0,
        Near = 1,
        Far = 2,
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct PointerType {
    pub alias: PointerAliasingRule,
    pub valid_range: Pair<ValidRangeType, u64>,
    pub decl: PointerDeclarationType,
    pub kind: PointerKind,
    pub addr_space: u32,
    pub inner: Box<Type>,
}

impl core::fmt::Display for PointerType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("*")?;

        if self.decl.contains(PointerDeclarationType::REF) {
            f.write_str("ref ")?;
        }
        if self.decl.contains(PointerDeclarationType::VOLATILE) {
            f.write_str("volatile ")?;
        }
        if self.decl.contains(PointerDeclarationType::CONST) {
            f.write_str("const ")?;
        }

        if self.alias.contains(PointerAliasingRule::UNIQUE) {
            f.write_str("unique ")?;
        }
        if self.alias.contains(PointerAliasingRule::READ_ONLY) {
            f.write_str("readonly ")?;
        }
        if self.alias.contains(PointerAliasingRule::NONNULL) {
            f.write_str("nonnull ")?;
        }

        if self.addr_space != 0 {
            f.write_fmt(format_args!("addr_space({}) ", self.addr_space))?;
        }

        match self.kind {
            PointerKind::Default => {}
            PointerKind::Near => f.write_str("near ")?,
            PointerKind::Far => f.write_str("far ")?,
            kind => panic!("{:?} is invalid", kind),
        }

        self.inner.fmt(f)
    }
}

#[repr(C)]
#[derive(Clone, Default, Debug, Hash, PartialEq, Eq)]
pub struct FnType {
    pub ret: Type,
    pub params: Vec<Type>,
    pub variadic: bool,
    pub tag: String,
}

impl core::fmt::Display for FnType {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("(")?;

        let mut params = self.params.iter();

        if let core::option::Option::Some(first) = params.next() {
            first.fmt(f)?;
            for param in params {
                f.write_str(", ")?;
                param.fmt(f)?;
            }

            if self.variadic {
                f.write_str(",...")?;
            }
        } else if self.variadic {
            f.write_str("...")?;
        }
        f.write_str(")")?;
        f.write_str(" -> ")?;
        self.ret.fmt(f)
    }
}

#[repr(u32)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StringEncoding {
    Utf8,
    Utf16BE,
    Utf32BE,
    Utf16LE,
    Utf32LE,
    Wtf8,
    Wtf16BE,
    Wtf16LE,
}

impl core::fmt::Display for StringEncoding {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Utf8 => f.write_str("utf8"),
            Self::Utf16LE => f.write_str("utf16le"),
            Self::Utf16BE => f.write_str("utf16be"),
            Self::Wtf8 => f.write_str("wtf8"),
            Self::Wtf16LE => f.write_str("wtf16le"),
            Self::Wtf16BE => f.write_str("wtf16be"),
            Self::Utf32LE => f.write_str("utf32le"),
            Self::Utf32BE => f.write_str("utf32be"),
        }
    }
}

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Value {
    Invalid(Type),
    Uninitialized(Type),
    GenericParameter(u32),
    Integer {
        ty: ScalarType,
        val: u128,
    },
    GlobalAddress {
        ty: Type,
        item: Path,
    },
    ByteString {
        content: Vec<u8>,
    },
    String {
        encoding: StringEncoding,
        utf8: String,
        ty: Type,
    },
    LabelAddress(u32),
    Empty,
}

impl Default for Value {
    fn default() -> Self {
        Value::Empty
    }
}

impl core::fmt::Display for Value {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Invalid(ty) => f.write_fmt(format_args!("invalid {}", ty)),
            Self::Uninitialized(ty) => f.write_fmt(format_args!("uninit {}", ty)),
            Self::GenericParameter(n) => f.write_fmt(format_args!("%{}", n)),
            Self::Integer { ty, val } => f.write_fmt(format_args!("{} {}", ty, val)),
            Self::GlobalAddress { ty, item } => {
                f.write_fmt(format_args!("global_address {} ({})", item, ty))
            }
            Self::ByteString { content } => match core::str::from_utf8(content) {
                Ok(s) => f.write_fmt(format_args!("byte \"{}\"", s.escape_default())),
                Err(mut err) => {
                    let mut bytes = &content[..];
                    f.write_str(" \"")?;
                    while !bytes.is_empty() {
                        let (left, right) = bytes.split_at(err.valid_up_to());
                        core::str::from_utf8(left)
                            .unwrap()
                            .escape_default()
                            .fmt(f)?;
                        if let core::option::Option::Some(len) = err.error_len() {
                            let (err_bytes, rest) = right.split_at(len);
                            bytes = rest;
                            for b in err_bytes {
                                f.write_fmt(format_args!("\\x{:02x}", b))?;
                            }
                        } else {
                            let err_bytes = core::mem::take(&mut bytes);
                            for b in err_bytes {
                                f.write_fmt(format_args!("\\x{:02x}", b))?;
                            }
                        }
                        match core::str::from_utf8(bytes) {
                            Ok(s) => {
                                s.escape_default().fmt(f)?;
                                break;
                            }
                            Err(next_err) => err = next_err,
                        }
                    }
                    f.write_str("\"")
                }
            },
            Self::String { encoding, utf8, ty } => f.write_fmt(format_args!(
                "{} {} \"{}\"",
                ty,
                encoding,
                utf8.escape_default()
            )),
            Self::LabelAddress(n) => f.write_fmt(format_args!("label_address @{}", n)),
            Self::Empty => Ok(()),
        }
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct CompareOp {
        Cmp = 0,
        CmpLt = 1,
        CmpGt = 2,
        CmpEq = 3,
        CmpNe = 4,
        CmpGe = 5,
        CmpLe = 6,
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct BinaryOp {
        Add = 0,
        Sub = 1,
        Mul = 2,
        Div = 3,
        Mod = 4,
        BitAnd = 5,
        BitOr = 6,
        BitXor = 7,
        Rsh = 8,
        Lsh = 9,


    }
}

impl core::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Self::Add => f.write_str("add"),
            Self::Sub => f.write_str("sub"),
            Self::Mul => f.write_str("mul"),
            Self::Div => f.write_str("div"),
            Self::Mod => f.write_str("mod"),
            Self::BitAnd => f.write_str("bit_and"),
            Self::BitOr => f.write_str("bit_or"),
            Self::BitXor => f.write_str("bit_xor"),
            Self::Rsh => f.write_str("rsh"),
            val => todo!("Invalid Operand {:?}", val),
        }
    }
}

impl core::fmt::Display for CompareOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            Self::CmpLt => f.write_str("cmp_lt"),
            Self::CmpGt => f.write_str("cmp_gt"),
            Self::CmpLe => f.write_str("cmp_le"),
            Self::CmpGe => f.write_str("cmp_ge"),
            Self::CmpEq => f.write_str("cmp_eq"),
            Self::CmpNe => f.write_str("cmp_ne"),
            Self::Cmp => f.write_str("cmp"),
            val => todo!("Invalid Operand {:?}", val),
        }
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct UnaryOp {
        Minus = 0,
        BitNot = 1,
        LogicNot = 2,
    }
}

impl core::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Self::Minus => f.write_str("minus"),
            Self::BitNot => f.write_str("bnot"),
            Self::LogicNot => f.write_str("not"),
            val => todo!("Invalid Operand {:?}", val),
        }
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct OverflowBehaviour{
        Wrap = 0,
        Trap = 1,
        Checked = 2,
        Unchecked = 3,
        Saturate = 4,
    }
}

impl core::fmt::Display for OverflowBehaviour {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Self::Wrap => f.write_str("wrap"),
            Self::Trap => f.write_str("trap"),
            Self::Checked => f.write_str("checked"),
            Self::Unchecked => f.write_str("unchecked"),
            Self::Saturate => f.write_str("saturate"),
            val => todo!("Invalid Overflow Behaviour {:?}", val),
        }
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct UnaryLValueOp{
        PreInc = 3,
        PostInc = 4,
        PreDec = 5,
        PostDec = 6,
    }
}

impl core::fmt::Display for UnaryLValueOp {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Self::PreInc => f.write_str("preinc"),
            Self::PostInc => f.write_str("postinc"),
            Self::PreDec => f.write_str("predec"),
            Self::PostDec => f.write_str("postdec"),
            val => panic!("Invalid Operation {:?}", val),
        }
    }
}

fake_enum::fake_enum! {
    #[repr(u16)]
    #[derive(Hash)]
    pub enum struct LValueOp{
        Xchg = 0,
        Cmpxchg = 1,
        Wcmpxchg = 2,
    }
}

impl core::fmt::Display for LValueOp {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match *self {
            Self::Xchg => f.write_str("xchg"),
            Self::Cmpxchg => f.write_str("cmpxchg"),
            Self::Wcmpxchg => f.write_str("wcmpxchg"),
            val => panic!("Invalid Operation {:?}", val),
        }
    }
}

#[repr(u16)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum BranchCondition {
    Always,
    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Never,
}

impl BranchCondition {
    pub const fn invert(self) -> BranchCondition {
        match self {
            Self::Always => Self::Never,
            Self::Less => Self::GreaterEqual,
            Self::LessEqual => Self::Greater,
            Self::Equal => Self::NotEqual,
            Self::NotEqual => Self::Equal,
            Self::Greater => Self::LessEqual,
            Self::GreaterEqual => Self::Less,
            Self::Never => Self::Always,
        }
    }
}

impl core::fmt::Display for BranchCondition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Always => f.write_str("always"),
            Self::Less => f.write_str("less"),
            Self::LessEqual => f.write_str("less_equal"),
            Self::Equal => f.write_str("equal"),
            Self::NotEqual => f.write_str("not_equal"),
            Self::Greater => f.write_str("greater"),
            Self::GreaterEqual => f.write_str("greater_equal"),
            Self::Never => f.write_str("never"),
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum ConversionStrength {
    Strong,
    Weak,
    Reinterpret,
}

impl core::fmt::Display for ConversionStrength {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Strong => f.write_str("strong"),
            Self::Weak => f.write_str("weak"),
            Self::Reinterpret => f.write_str("reinterpret"),
        }
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AggregateCtor {
    pub ty: Type,
    pub fields: Vec<String>,
}

impl core::fmt::Display for AggregateCtor {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.ty.fmt(f)?;
        f.write_str(" {")?;
        let mut sep = "";
        for field in &self.fields {
            f.write_str(sep)?;
            sep = ", ";
            f.write_str(field)?;
        }
        f.write_str("}")
    }
}

///
/// An xir expression/instruction
///
///
///
#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Expr {
    /// Sequences surrounding code according to class
    ///
    /// ## Stack
    ///
    /// Type checking: `[..]=>[..]`
    ///
    /// Operands; `[..]=>[..]`
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= "sequence" <access-class>
    /// ```
    ///
    /// ## Semantics
    ///
    /// 1. If `access-class` contains `volatile`, the expression performs a observable side effect (`[intro.abstract]#6`).
    /// 2. If `access-class` contains `nontemporal`, then all preceeding operations weekly-sequenced-before this expression *weekly-happens-before* this expression.
    /// 3. If `access-class` contains an atomic access class, the expression functions the same a fence expression with that atomic access class,
    ///   except that the fence does not *synchronize-with* atomic operations or fence instructions performed by another thread of execution.
    /// 4. [_Note: This is suitable for communicating with signal or interrupt handlers executed on the current thread._]
    /// 5. [_Note: The `access-class` modifier `freeze` may appear, but is ignored by this expression. _]
    /// 6. If `access-class` does not contain either `volatile`, `nontemporal`, or an atomic access class, the expression performs no operation.
    ///
    /// ### Platform Notes
    ///
    /// Typically, a `sequence` expr corresponds to no generated machine code, regardless of access-class.
    ///
    Sequence(AccessClass),
    /// Pushes a constant value.
    ///
    /// ## Stack
    ///
    /// Type checking: `[..]`=>`[..,T]``
    ///
    /// Operands: `[..]``=>`[..,Value]`
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= "const" <value
    /// ```
    ///
    /// ## Semantics
    /// Pushes a constant value, with no side effects.
    Const(Value),

    /// Computes the given binary Op
    ///
    /// ## Stack
    /// In all of the following, `S` means a scalar type, `P` means a pointer type `I` an integer type.
    /// Each occurance of `S`, `P`, or `I` in the same type checking specification refers to the same type.
    ///
    /// ### Form 1
    ///
    /// Type checking: `[..,S,S]`=>`[..,S]`
    ///
    /// Operands: `[..,a,b]`=>`[..,res]`
    ///
    /// (All except comparison ops or [`OverflowBehaviour::Checked`])
    ///
    /// ### Form 2
    ///
    /// Type checking: `[..,S, S]`=>`[..,S, uint(1)]`
    ///
    /// Operands: `[..,a,b]`=>`[..,res, v]`
    ///
    /// ([`OverflowBehaviour::Checked`], except for comarpison ops)
    ///
    /// `S` may not have a vectorsize
    ///
    /// ### Form 3
    ///
    /// Type checking: `[..,S,S]` => `[..,uint(1)]` or `[..,P,P]` => `[..,uint(1)]`
    ///
    /// Operands: `[..,a,b]`=>`[..,res]`
    ///
    /// (Comparison Op, other than [`BinaryOp::Cmp`])
    ///
    /// ### Form 4
    ///
    /// Type checking: `[..,S, S]`=>`[..,int(32)]` or `[..P,P]`=>`[..,int(32)]`
    ///
    /// Operands: `[..,a,b]`=>`[..,res]`
    ///
    /// ([`BinaryOp::Cmp`] except [`OverflowBehaviour::Checked`])
    ///
    /// ### Form 5
    ///
    /// Type checking: `[..,S, S]`=>`[..,int(32), uint(0)]`
    ///
    /// Operands: `[..,a,b]`=>`[..,res,unordered]`
    /// ([`BinaryOp::Cmp`] with [`OverflowBehaviour::Checked`])
    ///
    /// ### Form 6
    ///
    /// Type checking: `[..,P,I]`=>`[..,P]` or `[..,I,P]`=>`[..,P]` ([`BinaryOp::Add`] only)
    ///
    /// Operands: `[.., ptr, idx]`=>`[..,res]` or `[.., idx, ptr]`=>`[..,res]` ([`BinaryOp::Add`] only)
    ///
    /// [`BinaryOp::Add`] or [`BinaryOp::Sub`] only (pointer+integer)
    ///
    /// ### Form 7
    ///
    /// Type checking: `[..,P, P]`=>`[..,DI]`
    ///
    /// (`DI` is the signed integer type with width equal to the `size_bits` target property)
    ///
    /// Operands: `[.., a, b]`=>`[..,res]`
    ///
    /// [`BinaryOp::Sub`] only (pointer+pointer)
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= <binary-op> [<overflow-behaviour>]
    /// ```
    ///
    /// ## Semantics
    ///
    /// If any operand is `uninit`, all results are `uninit`.
    ///
    /// ### Form 1
    ///
    /// Computes the value of the specified binary-op with `a,b` according to `overflow-behaviour` (defaults to `wrap` if not specified)
    ///
    /// If an operation overflows (or an integer/fixed-point operation attempts to divide by zero), the result is the following, based on the specified `overflow-behaviour`:
    /// * `wrap`: Wraps modulo `2^N`. Division by zero yields `uninit`
    /// * `unchecked`: Yields `uninit`
    /// * `trap`: Causes abnormal program termination
    /// * `saturate`: Saturates to Minimum/Maxmimum value of the type. Division by zero yields a quotient of the maximum value, and a remainder of the minimum value of the type.
    ///
    /// Overflow in this section is not the same as floating-point overflow.
    /// Operations on floating-point or posit types are not considered to overflow for the purposes of this section, even if they would return +/-infinity from finite inputs or cause the `FE_OVERFLOW` floating-point exception.
    /// Operations on rationals or fixed-point values *can* cause overflow.
    ///
    /// If `overflow-behaviour` is `trap`, and any operand is uninit, the behaviour is undefined.
    ///
    /// ### Form 2
    ///
    /// Computes the value of the specified binary-op, checking for overflow or input errors.
    /// If an operation overflow (or an integer/fixed-point operation attempts to divide by zero),
    ///  the result is wrapped modulo 2^N (or saturated for `div`/`rem`) and the overflow flag (`v`) is `1` (else `0`).
    ///
    /// ### Form 3
    ///
    /// Compares two scalar or pointer values, and returns `0` if the comparison fails and `1` otherwise.
    ///
    /// `overflow-behaviour` is ignored, and may not be `checked`.
    ///
    /// ### Form 4
    ///
    /// Performs 3-way comparison on a scalar or pointer value, returning `-1` if `a<b`, `0` if `a==b`, or `1` if `a>b`.
    ///
    /// `overflow-behaviour` is ignored. If the comparison of `a,b` is unordered, the result is `uninit`.
    ///
    /// ### Form 5
    ///
    /// Performs 3-way comparison on a scalar value, returning `-1` if `a<b`, `0` if `a==b`, or `1` if `a>b`.
    ///
    /// If the comparison of `a,b` is unordered, the result is `uninit`. If the comparison is unordered, then `unord` is `1`, otherwise `unord` is `0`.
    ///
    /// ### Form 6
    ///
    /// Performs pointer addition/subtraction.
    ///
    /// Let `P` be a pointer to `T` where `T` is a complete value type.
    ///
    /// For `add`, the result is the pointer derived from `ptr` with address given by the offset from `ptr`
    ///  determined by the offset between an element `a` and the `idx`th following element `b`.
    ///
    /// For `sub`, the result is the pointer derived from `ptr` with address given by the offset from `ptr`
    ///  determined by the offset between an element `a` and the `idx`th previous element `b`.
    ///
    /// `overflow-behaviour` (default is `wrap`) must be either `wrap` or `unchecked`.
    ///
    /// If `overflow-behaviour` is `unchecked`, then the result is `uninit` if any of the following are true:
    /// * The resulting pointer as above does not point into an object
    /// * The resulting pointer as above points into a different complete object than `ptr`
    /// * The offset between the elements of the array is the value `off`, such that `off` cannot be exactly represented as a value of type `DI`
    /// * The resulting pointers address wraps the address space (for a far pointer) or segment (for a near pointer)
    ///
    /// (DI is the signed integer type with width equal to the `sizebits` target property)
    ///
    /// ### Form 7
    ///
    /// Performs pointer subtraction.
    ///
    /// Let `P` be a pointer to `T`, where `T` is a complete value type.
    ///
    /// Let the offset between the addresses of `b` and `a` be `off`.
    ///
    /// Returns `idx`, such that the offset between some element `A` and the `idx`th previous element `B`  is the value `off`.
    ///
    /// `overflow-behaviour` (default is `wrap`) must be either `wrap` or `unchecked`.
    ///
    /// If `overflow-behaviour` is `unchecked`, then the result is `uninit` if any of the following are true:
    /// * Either `a` or `b` do not point into an object
    /// * `a` and `b` do not point into the same complete object
    /// * Either `off` or `idx` cannot be represented as a value of type `DI`
    /// * The pointers are near pointers that refer to objects in different segments of the address space
    ///
    /// (DI is the signed integer type with width equal to the `sizebits` target property)
    BinaryOp(BinaryOp, OverflowBehaviour),
    /// Performs the given unary operation
    ///
    /// ## Stack
    /// In all of the following, `S` means a scalar type, `I` means an integer type.
    /// Each occurance of `S` or `I` in the same type checking specification refers to the same type.
    ///
    /// ### Form 1
    ///
    /// Type checking: `[..,S]` => `[..,S]`
    ///
    /// Operands: `[..,val]`=>`[..,res]`
    ///
    /// (Except for [`OverflowBehaviour::Checked`] or [`UnaryOp::LogicNot`])
    ///
    /// ### Form 2
    ///
    /// Type checking: `[..,S]`=>`[..,S, uint(1)]`
    ///
    /// Operands: `[..,val]`=>[..,res,v]
    ///
    /// ([`OverflowBehaviour::Checked`] except for [`UnaryOp::LogicNot`]))
    ///
    /// ### Form 3
    ///
    /// Type checking: `[..I]`=>`[..,I]`
    ///
    /// Operands: `[..,val]`=>`[..,val]`
    ///
    /// ([`UnaryOp::LogicNot`] only)
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= <unary-op> [<overflow-behaviour>]
    /// ```
    ///
    /// ## Semantics
    ///
    /// If `val` is `uninit`, all results are `uninit`.
    /// `overflow-behaviour` defaults to `wrap`
    ///
    /// ### Form 1
    ///
    /// Computes the given unary op on `val`, returning the result.
    ///
    /// If overflow occurs, the result is according to the `overflow-behaviour`:
    /// * `wrap`: The value is wrapped modulo 2^N
    /// * `unchecked`: The result is `uninit`.
    /// * `trap`: The program terminates abnormally
    /// * `saturate`: The value is saturated to the minimum/maximum value of the type.
    ///
    /// ### Form 2
    ///
    /// Computes the given unary op on `val` checking for overflow.
    ///
    /// If overflow occurs, the result is wrapped modulo `2^N` and `v` is 1.
    ///
    /// ### Form 3
    ///
    /// Computes the logical negation of `val`.
    /// If `val` is `0`, then the result is `1` if the integer type is unsigned, and `-1` if the integer type is signed.
    /// If `val` is non-zero then the result is `0`.
    ///
    /// If `val` cannot be represented as `I` (only possible for width `0` integer types), the result is `0`.
    ///
    /// The overflow-behaviour is ignored (overflow-behaviour cannot be `checked`)
    ///
    UnaryOp(UnaryOp, OverflowBehaviour),
    ///
    CompareOp(CompareOp, ScalarType),
    Convert(ConversionStrength, Type),
    Derive(PointerType, Box<Self>),
    /// Obtains an lvalue that designates the specified local variable
    ///
    /// ## Stack
    ///
    /// Type checking: `[..]`=>`[..,lvalue T]`
    /// Operands: `[..]`=>`[..,local]`
    ///
    /// ## Syntax
    /// ```abnf
    /// local-id = <IDENT>
    ///
    /// expr :/= "local" <local-id>
    /// ```
    ///
    /// where `local-id` matches `"_"*<dec-digit>`
    ///
    /// ## Semantics
    ///
    /// Pushes an lvalue that designates the specified `<local-id>` defined in the current function.
    ///
    /// Ill-formed if `<local-id>` is not defined in the function
    ///
    Local(u32),
    /// The stack `pop n` operation
    ///
    /// ## Stack
    ///
    /// Type checking: `[..,T0,T1, ..., Tn]`=>`[..]`
    ///
    /// Operands: `[..,i0,i1, ..., in]`=>`[..]`
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= "pop" [<int-literal>]
    /// ```
    ///
    /// ## Semantics
    ///
    /// For `pop n`, pops `n` values from the stack and discards them with no side effects.
    ///
    /// `pop` is an alias for `pop 1`.
    ///
    Pop(u32),
    /// The stack `dup n` operation
    ///
    /// ## Stack
    ///
    /// Type checking: `[.., T0, T1, ..., Tn]` => `[..,T0,T1, ..., Tn, T0, T1, ..., Tn]`
    ///
    /// Operands: `[..,v0,v1, ..., vn]`=>`[..,v0,v1, ..., vn,v0,v1, ..., vn]`
    ///
    /// ## Syntax
    /// ```abnf
    /// expr /= "dup" [<int-literal>]
    /// ```
    ///
    /// ## Semantics
    ///
    /// For `dup n`, pops `n` values from the stack, then pushes those `n` values twice in order.
    ///
    /// `dup` is an alias for `dup 1`
    Dup(u32),
    /// The stack `pivot m n` operation
    ///
    /// ## Stack
    ///
    /// Type checking: `[.., S0, S1,  ..., Sm, T0, T1, ..., Tn]`=> `[.., T0, T1, ..., Tn, S0, S1, ..., Sm]`
    ///
    /// Operands: `[.., s0, s1, ..., sm, t0, t1, ..., tn]`=>`[.., t0, t1, ..., tn, s0, s1, ..., sm]`
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= "pivot" [<int-literal> [<int-literal>]]
    /// ```
    ///
    /// ## Semantics
    ///
    /// For `pivot m n`, pops `n` values from the stack, preserves them in an unspecified location, pops `m` additional values,
    ///  then pushes the first set of values popped followed by the second set of values popped.
    Pivot(u32, u32),
    Aggregate(AggregateCtor),
    /// Projects to a field of the type of an aggregate
    ///
    /// ## Stack
    ///
    /// In all of the following, `A` is an aggregate type or a product type. `F` is the type of the named field of `A`.
    ///
    /// The expression is invalid if `A` does not have the field named by `member-name`.
    ///
    /// ### Form 1
    /// Type checking: `[.., A]`=>`[..,F]`
    ///
    /// Operands: `[..,a]`=>`[..,f]`
    ///
    /// ### Form 2
    /// Type checking: `[.., lvalue A]`=>`[..,lvalue F]`
    ///
    /// Operands: `[..,a]`=>`[..,f]`
    ///
    /// ## Syntax
    /// ```abnf
    /// member-name := <ident> / <int-literal> / "(" <member-name> ")"
    /// expr /= "member" <member-name>
    /// ```
    ///
    /// `member-name` may not be `indirect` without parenthesis.
    ///
    /// ## Semantics
    ///
    /// Projects to a named member of an aggregate type or product type.
    ///
    /// ### Form 1
    ///
    /// The operand and the result are both rvalues.
    /// Returns the value of the named member.
    ///
    /// ### Form 2
    /// The operand and the result are both lvalues.
    /// Returns an lvalue that designates the named member of `a`.
    /// If the member is a bitfield, then the resulting lvalue is an lvalue that designates a bitfield.
    ///
    /// The program is ill-formed if an lvalue that designates a bitfield is used in any of the following ways:
    /// * As an operand to a `compound_assign`, `fetch_assign`, `assign`, `as_rvalue`, lvalue op, or unary lvalue op with an atomic access class
    /// * As the operand of a `member` or `addr_of` expression
    /// * As a value used as an incoming value for a jump target
    Member(String),
    /// Projects a Pointer to a field of an aggregate
    ///
    /// ## Stack
    /// Type Checking: `[.., *A]`=> `[.., *F]`  
    /// Operands: `[.., a]` => `[.., f]`
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= "member" "indirect" <member-name>
    /// ```
    ///
    /// ## Semantics
    /// Given `a` is a pointer to a struct, yeilds `f`, a pointer to the named member of `a`.
    ///
    /// If `member-name` designates a bitfield, the program is ill-formed.
    ///
    /// The behaviour is undefined unless:
    /// * `a` and `f` are the same pointer, or
    /// * `a` is a valid pointer, and `f` is inbounds of the same complete object as `a`.
    ///
    /// The operation is an implicit derive operation if `a` and `f` are not the same pointer, or if `A` has any non-trivial validity attributes.
    ///
    MemberIndirect(String),
    /// Assigns a value to an lvalue according to `class`
    ///
    /// ## Stack
    /// Type Checking: `[.., T, lvalue T]`=>`[..]`
    ///
    /// Operands: `[..,v, t]`=>`[..]`.
    ///
    /// ## Syntax:
    /// ```abnf
    /// expr /= "assign" <access-class>
    /// ```
    ///
    /// ## Semantics
    ///
    /// Assigns the value `v` to the object designated by `t`, performing the memory operation according to `class`.
    Assign(AccessClass),

    /// Reads the value designated by an lvalue according to `class`
    ///
    /// ## Stack
    /// Type Checking: `[.., lvalue T]`=>`[..,T]`
    ///
    /// Operands: `[.., t]`=>`[.., v]`.
    ///
    /// ## Syntax
    ///```abnf
    /// expr /= "as_rvalue" <access-class>
    /// ```
    ///
    /// ## Semantics
    ///
    /// Reads the value of the object designated by `t`, performing the memory operation according to `class`.
    ///
    AsRValue(AccessClass),
    CompoundAssign(BinaryOp, OverflowBehaviour, AccessClass),
    FetchAssign(BinaryOp, OverflowBehaviour, AccessClass),
    LValueOp(LValueOp, AccessClass),
    UnaryLValue(UnaryLValueOp, OverflowBehaviour, AccessClass),
    Indirect,
    AddrOf,

    /// Establishes ordering between threads of execution.
    ///
    /// ## Stack
    /// `[..]`=>`[..]`
    ///
    /// ## Syntax
    ///
    /// ```abnf
    /// expr /= "fence" <access-class>
    /// ```
    ///
    /// ## Semantics
    ///
    /// 1. An *acquire fence* is a fence with an access-class containing an atomic-class of `atomic acquire`, `atomic acq_rel`, or `atomic seq_cst`.
    /// 2. A *release fence* is a fence with an access-class containing an atomic-class of `atomic release`, `atomic acq_rel`, or `atomic seq_cst`.
    /// 3. A fence with an access class containing `volatile` performs an observable side effect.
    /// 4. An *acquire fence* M *synchronizes-with* a *release fence* N, if there exists two atomic operations A and B, such that:
    ///   * A is *sequenced-before* M,
    ///   * N is *sequenced-before* B,
    ///   * A is a load, B is a store, and A takes its value from B or any store in a hypothetical release sequence that would have been headed by B if B was a release operation
    /// 5. An *acquire fence* M *synchronizes-with* a *release operation* N, if there exists an atomic operation A, such that:
    ///   * A is *sequenced before* M, and
    ///   * A is a load and takes its value from a N, or any store in the release sequence headed by N.
    /// 6. An *acquire operation* M *synchronizes-with* a *release fence* N, if there exists an atomic operation A, such that:
    ///   * N is *sequenced before* A, and
    ///   * A is a store, and M takes its value from A, or any store in a hypothetical release sequence that would have been headed by B if B was a release operation.
    /// 7. A fence with an access-class containing an atomic-class of `atomic seq_cst` participates in `S`.
    /// 8. If a fence `F` has an access-class containing `nontemporal`, then any operation which is *weekly sequenced before* `F` *happens before* `F`.
    /// 9. [_Note: The access-class `freeze` may be present but has no effect.]
    Fence(AccessClass),

    BeginStorage(u32),
    EndStorage(u32),

    Select(u32),
}

impl Expr {
    // Don't break existing code
    #[allow(non_upper_case_globals)]
    pub const Null: Self = Self::Sequence(AccessClass::Normal);
}

impl Default for Expr {
    fn default() -> Self {
        Self::Sequence(AccessClass::Normal)
    }
}

impl core::fmt::Display for Expr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Const(val) => {
                f.write_str("const ")?;
                val.fmt(f)
            }
            Self::CompareOp(op, sty) => f.write_fmt(format_args!("{} {}", op, sty)),
            Self::BinaryOp(op, v) => f.write_fmt(format_args!("{} {}", op, v)),
            Self::UnaryOp(op, v) => f.write_fmt(format_args!("{} {}", op, v)),
            Self::Convert(strength, ty) => f.write_fmt(format_args!("convert {} {}", strength, ty)),
            Self::Derive(pty, inner) => f.write_fmt(format_args!("derive {} {}", pty, inner)),
            Self::Local(n) => f.write_fmt(format_args!("local _{}", n)),
            Self::Pop(n) => f.write_fmt(format_args!("pop {}", n)),
            Self::Dup(n) => f.write_fmt(format_args!("dup {}", n)),
            Self::Pivot(m, n) => f.write_fmt(format_args!("pivot {} {}", m, n)),
            Self::Aggregate(ctor) => f.write_fmt(format_args!("aggregate {}", ctor)),
            Self::Member(m) => {
                if m == "indirect" {
                    f.write_str("member (indirect)")
                } else {
                    f.write_fmt(format_args!("member {}", m))
                }
            }
            Self::MemberIndirect(m) => {
                if m == "indirect" {
                    f.write_str("member indirect (indirect)")
                } else {
                    f.write_fmt(format_args!("member indirect {}", m))
                }
            }
            Self::Assign(acc) => f.write_fmt(format_args!("assign {}", acc)),
            Self::AsRValue(acc) => f.write_fmt(format_args!("as_rvalue {}", acc)),
            Self::CompoundAssign(op, v, acc) => {
                f.write_fmt(format_args!("compound_assign {} {} {}", op, v, acc))
            }
            Self::FetchAssign(op, v, acc) => {
                f.write_fmt(format_args!("fetch_assign {} {} {}", op, v, acc))
            }
            Self::LValueOp(op, acc) => f.write_fmt(format_args!("{} {}", op, acc)),
            Self::UnaryLValue(op, v, acc) => f.write_fmt(format_args!("{} {} {}", op, v, acc)),
            Self::Indirect => f.write_str("indirect"),
            Self::AddrOf => f.write_str("addr_of"),
            Self::Sequence(AccessClass::Normal) => f.write_str("nop"),
            Self::Sequence(acc) => f.write_fmt(format_args!("sequence {}", acc)),
            Self::Fence(acc) => f.write_fmt(format_args!("fence {}", acc)),
            Self::BeginStorage(n) => f.write_fmt(format_args!("begin storage _{}", n)),
            Self::EndStorage(n) => f.write_fmt(format_args!("end storage _{}", n)),
            Self::Select(n) => f.write_fmt(format_args!("select {}", n)),
        }
    }
}

bitflags::bitflags! {
    /// The flags for a jump target
    ///
    /// Matches the syntax:
    /// ```abnf
    /// jump-target-flag := "fallthrough" / "cold" / "continue"
    /// ```
    #[repr(transparent)]
    #[derive(Default)]
    pub struct JumpTargetFlags : u32{
        /// The "fallthrough" flag.
        /// Indicates that the jump does not perform a branch but instead continues on to the next basic block
        ///
        /// The behaviour is undefined if the `target` field of the [`JumpTarget`] is not the immediately adjacent basic block
        const FALLTHROUGH = 1;
        /// The "cold" flag.
        /// Indicates that the branch is unlikely to be taken.
        ///
        /// The optimizer may pessimize the case where the branch is taken, to optimize the case where a different branch is taken
        const COLD = 2;
    }
}

impl core::fmt::Display for JumpTargetFlags {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.contains(Self::FALLTHROUGH) {
            f.write_str("fallthrough ")?;
        }
        if self.contains(Self::COLD) {
            f.write_str("cold ")?;
        }
        Ok(())
    }
}

/// The target of a jump, such as a branch,
/// Matches the syntax
/// ```abnf
/// jump-target := [*(<jump-target-flags>)] @<int-literal>
/// ```
#[repr(C)]
#[derive(Default, Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub struct JumpTarget {
    /// The flags for the jump
    pub flags: JumpTargetFlags,
    /// The target basic block
    pub target: u32,
}

impl core::fmt::Display for JumpTarget {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        self.flags.fmt(f)?;
        f.write_str("@")?;
        self.target.fmt(f)
    }
}

bitflags::bitflags! {
    /// Flags for the call instruction
    #[repr(transparent)]
    pub struct CallFlags: u32{
        /// Indicates the the call or tailcall will definitely return in finite time
        const WILLRETURN = 1;
    }
}

impl core::fmt::Display for CallFlags {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.contains(Self::WILLRETURN) {
            f.write_str("willreturn ")?;
        }
        Ok(())
    }
}

/// A terminator of a [`Block`]
///
/// Matchs the `terminator` production
#[repr(u32)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Terminator {
    /// Jump to another basic block, uncondtionally
    /// Matches the syntax
    /// ```abnf
    /// terminator := "jump" <jump-target>
    /// ```
    Jump(JumpTarget),
    /// Branch to one of two basic blocks, depending on the evaluation of a condition,
    Branch(BranchCondition, JumpTarget, JumpTarget),
    BranchIndirect,
    Call(CallFlags, Box<FnType>, JumpTarget),
    Tailcall(CallFlags, Box<FnType>),
    Exit(u16),
    Asm(AsmExpr),
    Switch(Switch),
    Unreachable,
    Empty,
}

impl Default for Terminator {
    fn default() -> Self {
        Self::Empty
    }
}

#[repr(u16)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum AsmConstraint {
    CC,
    Memory,
    ArchConstraint(String),
}

impl core::fmt::Display for AsmConstraint {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::CC => f.write_str("cc"),
            Self::Memory => f.write_str("memory"),
            Self::ArchConstraint(name) => f.write_str(name),
        }
    }
}

bitflags::bitflags! {
    #[repr(transparent)]
      // more like clippy::readable_literal
    pub struct AsmOptions : u32{
        #[allow(clippy::unreadable_literal)]
        const PURE = 0x00000001;
        #[allow(clippy::unreadable_literal)]
        const TRANSPARENT = 0x00000002;
        #[allow(clippy::unreadable_literal)]
        const NOSTACK = 0x00000004;
        #[allow(clippy::unreadable_literal)]
        const NOMEM = 0x00000008;
        #[allow(clippy::unreadable_literal)]
        const NOEXIT = 0x00000010;
    }
}

impl core::fmt::Display for AsmOptions {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.contains(Self::PURE) {
            f.write_str("pure ")?;
        }
        if self.contains(Self::TRANSPARENT) {
            f.write_str("transparent ")?;
        }
        if self.contains(Self::NOSTACK) {
            f.write_str("nostack ")?;
        }
        if self.contains(Self::NOMEM) {
            f.write_str("nomem ")?;
        }
        if self.contains(Self::NOEXIT) {
            f.write_str("noexit ")?;
        }
        Ok(())
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AsmOutput {
    pub late: bool,
    pub constraint: AsmConstraint,
    pub ty: Type,
}

impl core::fmt::Display for AsmOutput {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if self.late {
            f.write_str("late ")?;
        }

        self.constraint.fmt(f)?;

        f.write_str(" => ")?;

        self.ty.fmt(f)
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AsmExpr {
    pub opts: AsmOptions,
    pub syntax: String,
    pub access_class: AccessClass,
    pub string: String,
    pub clobbers: Vec<AsmConstraint>,
    pub targets: Vec<JumpTarget>,
    pub inputs: Vec<AsmConstraint>,
    pub outputs: Vec<AsmOutput>,
    pub next: Option<JumpTarget>,
}

impl core::fmt::Display for AsmExpr {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        f.write_str("asm ")?;
        self.opts.fmt(f)?;
        f.write_fmt(format_args!("syntax({}) ", self.syntax))?;

        if self.access_class != AccessClass::Normal {
            f.write_fmt(format_args!("class({}) ", self.access_class))?;
        }

        f.write_fmt(format_args!("{:?}", self.string))?;

        if !self.clobbers.is_empty() {
            f.write_str(" clobbers(")?;
            let mut sep = "";
            for clobber in &self.clobbers {
                f.write_str(sep)?;
                sep = ", ";
                clobber.fmt(f)?;
            }
            f.write_str(")")?;
        }

        if !self.targets.is_empty() {
            f.write_str(" goto(")?;
            let mut sep = "";
            for target in &self.targets {
                f.write_str(sep)?;
                sep = ", ";
                f.write_fmt(format_args!("@{}", target))?;
            }
            f.write_str(")")?;
        }

        f.write_str(" [")?;
        let mut sep = "";
        for input in &self.inputs {
            f.write_str(sep)?;
            sep = ", ";
            input.fmt(f)?;
        }

        f.write_str("] [")?;
        let mut sep = "";
        for output in &self.outputs {
            f.write_str(sep)?;
            sep = ", ";
            output.fmt(f)?;
        }
        f.write_str("]")
    }
}

fake_enum::fake_enum! {
    #[repr(pub u8)]
    #[derive(Hash)]
    /// ## Access Class [expr.class]
    ///
    /// ### Syntax
    /// ```abnf
    /// atomic-class := "relaxed" / "release" / "acquire" / "acq_rel" / "seq_cst"
    ///
    /// access-class-modifier := "volatile" / "nontemporal" / "freeze"
    ///
    /// access-class := [*<access-class-modifier>] ["atomic" <atomic-class>]
    /// ```
    ///
    /// ### Memory Ordering [expr.order]
    ///
    /// 1. This section defines the relations *weekly sequenced before*, *sequenced before*, *happens before*, and *weekly happens before*.
    ///  These are all partial ordering relations on the evaluations of two expressions during the execution of a program. The relations are asymmetric and transitive.
    /// 1. An expression A is *weekly sequenced before* another expression B if A and B are evaluated on the same thread of execution,
    /// either A and B are both not evaluated during the execution of a signal handler or A and B are both evaluated during the execution of the same signal handler,
    ///  and A is evaluated before B in program order,
    /// 2. An expression A is *sequenced before* another expression B if A is *weekly sequenced before* B, and either,
    ///    * Neither A nor B is a memory operation with the `nontemporal` access class,
    ///    * A and B are both memory operations, one has the `nontemporal` access-class, one is a store to a memory location, and the other is an access to the same memory location,
    ///    * A is a `nontemporal` memory operation, and B is a `fence` instruction with the `nontemporal` access-class, or
    ///    * A is *sequenced before* some expression C, and C is *sequenced before* B.
    /// 3. _Note: Absent memory operations using the nontemporal access class, *weekly sequenced before* and *sequenced before* are the same relation._
    /// 4. An expression A *happens before* another expression B, if:
    ///    * A is *sequenced before* B,
    ///    * A *synchronizes-with* B, or
    ///    * There exists an expression C, such that A *happens before* C and C *happens before* B.
    /// 5. An expression A *weekly happens before* another expression B, if:
    ///    * A *happens before* B,
    ///    * A is a memory operation with a nontemporal access class, B is a `sequence` instruction, and A is *weekly sequenced before* B,
    ///    * There exists an expression C, such that A *weekly happens before* C, and C *weekly happens before* B.
    /// 6. _Note: Weekly happens before allows a `sequence` instruction to synchronize `nontemporal` accesses with _
    ///
    pub enum struct AccessClass{
        Normal = 0,
        AtomicRelaxed = 1,
        AtomicRelease = 2,
        AtomicAcquire = 3,
        AtomicAcqRel = 4,
        AtomicSeqCst = 5,
    }
}

#[allow(non_upper_case_globals)]
impl AccessClass {
    pub const Volatile: Self = Self(0x10);
    pub const Nontemporal: Self = Self(0x20);
    pub const Freeze: Self = Self(0x40);

    pub const ATOMIC_MASK: Self = Self(0xf);
}

impl std::fmt::Display for AccessClass {
    #[allow(clippy::useless_let_if_seq)]
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let atomic = self.0 & 0xf;
        let bits = self.0 & 0xf0;
        let mut sep = "";
        if (bits & 0x10) != 0 {
            f.write_str("volatile")?;
            sep = " ";
        }
        if (bits & 0x20) != 0 {
            f.write_str(sep)?;
            f.write_str("nontemporal")?;
            sep = " ";
        }
        if (bits & 0x40) != 0 {
            f.write_str(sep)?;
            f.write_str("freeze")?;
            sep = " ";
        }

        match atomic {
            0 => {}
            1 => f.write_fmt(format_args!("{}atomic relaxed", sep))?,
            2 => f.write_fmt(format_args!("{}atomic release", sep))?,
            3 => f.write_fmt(format_args!("{}atomic acquire", sep))?,
            4 => f.write_fmt(format_args!("{}atomic acq_rel", sep))?,
            5 => f.write_fmt(format_args!("{}atomic seq_cst", sep))?,
            x @ 6..=16 => panic!("unknown atomic({})", x),
            _ => unreachable!(),
        }

        Ok(())
    }
}

impl BitOr for AccessClass {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self(self.0 | rhs.0)
    }
}

impl BitAnd for AccessClass {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        Self(self.0 & rhs.0)
    }
}

impl BitOrAssign for AccessClass {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}
impl BitAndAssign for AccessClass {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[repr(u32)]
pub enum Switch {
    Hash(HashSwitch),
    Linear(LinearSwitch),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct HashSwitch {
    pub cases: Vec<Pair<Value, JumpTarget>>,
    pub default: JumpTarget,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct LinearSwitch {
    pub ty: Type,
    pub min: u128,
    pub scale: u32,
    pub default: JumpTarget,
    pub cases: Vec<JumpTarget>,
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum StackValueKind {
    LValue,
    RValue,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct StackItem {
    pub ty: Type,
    pub kind: StackValueKind,
}

impl core::fmt::Display for StackItem {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        if StackValueKind::LValue == self.kind {
            f.write_str("lvalue ")?;
        }

        self.ty.fmt(f)
    }
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct Block {
    pub target: u32,
    pub incoming_stack: Vec<StackItem>,
    pub expr: Vec<Expr>,
    pub term: Terminator,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct FunctionBody {
    pub locals: Vec<Type>,
    pub blocks: Vec<Block>,
}

#[repr(C)]
#[derive(Clone, Debug, Hash, PartialEq, Eq, Default)]
pub struct FunctionDeclaration {
    pub ty: FnType,
    pub linkage: Linkage,
    pub body: Option<FunctionBody>,
}

#[repr(C)]
#[derive(Clone, Debug)]
pub struct File {
    pub target: String,
    pub root: Scope,
}

impl core::fmt::Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut wrapper = fmt::IrFormatter::new(f);

        wrapper.write_fmt(format_args!("target {};", self.target))?;
        let tabs = fmt::Tabs::new();

        for Pair(p, mem) in &self.root.members {
            wrapper.fmt_scope_member(mem, p, tabs)?;
        }
        Ok(())
    }
}
