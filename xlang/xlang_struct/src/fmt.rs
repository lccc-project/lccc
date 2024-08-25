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

    pub fn fmt_terminator(&mut self, term: &Terminator, tabs: Tabs) -> core::fmt::Result {
        tabs.fmt(self)?;
        match term {
            Terminator::Jump(jump) => self.write_fmt(format_args!("jump {}", jump)),
            Terminator::Branch(cond, then_targ, else_targ) => self.write_fmt(format_args!(
                "branch {} {} else {}",
                cond, then_targ, else_targ
            )),
            Terminator::BranchIndirect => self.write_str("branch indirect"),
            Terminator::Call(flags, fnty, next) => {
                self.write_fmt(format_args!("call {}function{} next {}", flags, fnty, next))
            }
            Terminator::Tailcall(flags, fnty) => {
                self.write_fmt(format_args!("tailcall {}function{}", flags, fnty))
            }
            Terminator::Exit(vals) => self.write_fmt(format_args!("exit {}", vals)),
            Terminator::Asm(asm) => asm.fmt(self),
            Terminator::Switch(switch) => {
                self.write_str("switch ")?;
                let nested = tabs.nest();
                match switch {
                    Switch::Hash(switch) => {
                        self.write_str("hash\n")?;
                        for case in &switch.cases {
                            self.write_fmt(format_args!("{}{}: {}\n", nested, case.0, case.1))?;
                        }
                        self.write_fmt(format_args!("{}default {}\n", nested, switch.default))?;
                    }
                    Switch::Linear(switch) => {
                        self.write_fmt(format_args!(
                            "linear {} min {} scale {}\n",
                            switch.ty, switch.min, switch.scale
                        ))?;
                        for case in &switch.cases {
                            self.write_fmt(format_args!("{}{}", nested, case))?;
                        }
                        self.write_fmt(format_args!("{}default {}\n", nested, switch.default))?;
                    }
                }
                self.write_fmt(format_args!("{}end switch", tabs))
            }
            Terminator::Unreachable => self.write_str("unreachable"),
            Terminator::Empty => Ok(()),
        }
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

                    for block in &body.blocks {
                        nested.fmt(self)?;
                        self.write_str("target @")?;
                        block.target.fmt(self)?;
                        self.write_str(" [")?;
                        let mut sep = "";
                        for item in &block.incoming_stack {
                            self.write_str(sep)?;
                            sep = ", ";
                            item.fmt(self)?;
                        }
                        self.write_str("]{\n")?;
                        let inner_nest = nested.nest();

                        for expr in &block.expr {
                            inner_nest.fmt(self)?;
                            expr.fmt(self)?;
                            self.write_str("\n")?;
                        }

                        self.fmt_terminator(&block.term, inner_nest)?;
                        self.write_str("\n")?;
                        nested.fmt(self)?;
                        self.write_str("}\n")?;
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
