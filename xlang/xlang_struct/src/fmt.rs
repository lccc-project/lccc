//! Helper types for pretty printing `xlang_struct` types
use crate::*;

use core::fmt::Display;
use core::ops::{Deref, DerefMut};

/// A type that formats as some number of tab stops or equivalent indendation
#[derive(Copy, Clone)]
pub struct Tabs(u64);

impl Tabs {
    /// Creates a new [`Tabs`] value that formats to `0` tab stops
    pub const fn new() -> Self {
        Self(0)
    }
    /// Creates a new [`Tabs`] value that formats as 1 more tab stop than the current instance.
    /// This can be used for nested values
    pub const fn nest(&self) -> Self {
        Self(self.0 + 1)
    }
}

impl Display for Tabs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _ in 0..self.0 {
            f.write_str("\t")?;
        }

        Ok(())
    }
}

/// A type that manages state for formatting `xlang_struct` types.
pub struct IrFormatter<'b, 'a>(&'b mut core::fmt::Formatter<'a>);

impl<'b, 'a> Deref for IrFormatter<'b, 'a> {
    type Target = core::fmt::Formatter<'a>;
    fn deref(&self) -> &Self::Target {
        self.0
    }
}

impl<'b, 'a> DerefMut for IrFormatter<'b, 'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.0
    }
}

impl<'b, 'a> IrFormatter<'b, 'a> {
    /// Creates a new [`IrFormatter`] that writes to `f`
    pub fn new(f: &'b mut core::fmt::Formatter<'a>) -> Self {
        Self(f)
    }

    /// Formats a scope member `mem` at the given `path` at the current nesting level given by `tabs`
    pub fn fmt_scope_member(
        &mut self,
        mem: &ScopeMember,
        p: &Path,
        tabs: Tabs,
    ) -> core::fmt::Result {
        tabs.fmt(self)?;

        mem.annotations.fmt(self)?;
        self.write_str("\n")?;
        tabs.fmt(self)?;
        mem.vis.fmt(self)?;

        match &mem.member_decl {
            MemberDeclaration::Empty => self.write_str(";\n"),
            MemberDeclaration::Scope(s) => {
                self.write_str("scope ")?;
                p.fmt(self)?;
                self.write_str("{\n")?;
                let nested = tabs.nest();
                for Pair(name, mem) in &s.members {
                    self.fmt_scope_member(mem, name, nested)?;
                }
                tabs.fmt(self)?;
                self.write_str("}\n")
            }
            MemberDeclaration::Function(f) => {
                self.write_fmt(format_args!("{}", f.linkage))?;
                self.write_str(" function ")?;
                p.fmt(self)?;
                f.ty.fmt(self)?;

                if let Some(body) = &f.body {
                    self.write_str("{\n")?;
                    let nested = tabs.nest();
                    for (local, ty) in body.locals.iter().enumerate() {
                        nested.fmt(self)?;
                        self.write_fmt(format_args!("declare _{}: {};\n", local, ty))?;
                    }

                    for item in &body.block.items {
                        nested.fmt(self)?;
                        item.fmt(self)?;
                        self.write_str("\n")?;
                    }

                    tabs.fmt(self)?;
                    self.write_str("}\n")
                } else {
                    self.write_str(";\n")
                }
            }
            MemberDeclaration::OpaqueAggregate(kind) => {
                match kind {
                    AggregateKind::Struct => self.write_str("struct ")?,
                    AggregateKind::Union => self.write_str("union ")?,
                }

                p.fmt(self)?;
                self.write_str(";\n")
            }
            MemberDeclaration::AggregateDefinition(def) => {
                match def.kind {
                    AggregateKind::Struct => self.write_str("struct ")?,
                    AggregateKind::Union => self.write_str("union ")?,
                }

                def.annotations.fmt(self)?;
                p.fmt(self)?;
                self.write_str("{\n")?;
                let nested = tabs.nest();

                for fields in &def.fields {
                    self.write_fmt(format_args!("{}{},\n", nested, fields))?;
                }
                tabs.fmt(self)?;
                self.write_str("}\n")
            }
            MemberDeclaration::Static(st) => {
                self.write_fmt(format_args!(
                    "{} static {}{}: {}",
                    st.linkage, st.specifiers, p, st.ty
                ))?;

                if st.init != Value::Empty {
                    self.write_fmt(format_args!("= {}", st.init))?;
                }

                self.write_str(";\n")
            }
        }
    }
}
