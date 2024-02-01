// #![deny(missing_docs, warnings)] // No clippy::nursery
#![allow(dead_code)] // I'm not deleting a bunch of randomly placed shit
//! A helper crate for implementing [`xlang::plugin::XLangCodegen`]s without duplicating code (also can be used to evaluate constant expressions)
//! the `xlang_backend` crate provides a general interface for writing expressions to an output.

use std::rc::Rc;

use arch_ops::traits::InsnWrite;
use mach::Machine;
use ssa::FunctionBuilder;
use ty::TypeInformation;
use xlang::{
    abi::{io::WriteAdapter, option::Some as XLangSome, pair::Pair, try_},
    ir::{self, Linkage},
    plugin::{XLangCodegen, XLangPlugin},
    targets::properties::{StackAttributeControlStyle, TargetProperties},
};

use binfmt::{
    fmt::{Section, SectionFlag, SectionType},
    sym::{Symbol, SymbolKind, SymbolType},
};

/// Module for handling and internalizing string literal values
pub mod str;

/// Module for handling the results of evaluating Expressions, and tracking the location of values
pub mod expr;

/// Module for handling types
pub mod ty;

/// Module for handling xlang/language intrinsics
pub mod intrinsic;

/// Module for handling calling convention, and calling functions
pub mod callconv;

/// Module for name mangling
pub mod mangle;

/// Module for Machine Support
pub mod mach;

/// Module for building SSA from XIR that can be readily lowered to machine code
/// Does not use FunctionCodegen
pub mod ssa;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SectionSpec {
    Global,
}

impl core::fmt::Display for SectionSpec {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::Global => Ok(()),
        }
    }
}

pub struct FunctionDef<M> {
    section: SectionSpec,
    linkage: Linkage,
    fnty: Rc<ir::FnType>,
    body: Option<FunctionBuilder<M>>,
}

pub struct SsaCodegenPlugin<M> {
    mach: Rc<M>,
    targ: Option<&'static TargetProperties<'static>>,
    functions: Vec<(String, FunctionDef<M>)>,
}

impl<M> SsaCodegenPlugin<M> {
    pub fn new(mach: M) -> Self {
        Self {
            mach: Rc::new(mach),
            targ: None,
            functions: Vec::new(),
        }
    }
}

impl<M: Machine> XLangPlugin for SsaCodegenPlugin<M> {
    fn accept_ir(
        &mut self,
        ir: &mut ir::File,
    ) -> xlang::abi::result::Result<(), xlang::plugin::Error> {
        let targ = self.targ.expect("set_target must be called first");
        let mut tys = TypeInformation::from_properties(targ);
        for Pair(path, field) in &ir.root.members {
            match &field.member_decl {
                ir::MemberDeclaration::Scope(_) => todo!("non-root scope"),
                ir::MemberDeclaration::OpaqueAggregate(_) => tys.add_opaque_aggregate(path.clone()),
                ir::MemberDeclaration::AggregateDefinition(def) => {
                    tys.add_aggregate(path.clone(), def.clone())
                }
                ir::MemberDeclaration::Function(_)
                | ir::MemberDeclaration::Static(_)
                | ir::MemberDeclaration::Empty => {}
            }
        }

        let tys = Rc::new(tys);
        for Pair(path, field) in &ir.root.members {
            match &field.member_decl {
                ir::MemberDeclaration::Scope(_) => todo!("non-root scope"),
                ir::MemberDeclaration::Function(f) => {
                    let sym = match &*path.components {
                        [ir::PathComponent::Text(name)]
                        | [ir::PathComponent::Root, ir::PathComponent::Text(name)] => {
                            name.to_string()
                        }
                        [ir::PathComponent::Root, rest @ ..] | [rest @ ..] => {
                            self.mach.mangle(rest)
                        }
                    };
                    let ty = Rc::new(f.ty.clone());

                    let section = SectionSpec::Global;
                    let linkage = f.linkage;
                    let body = if let XLangSome(body) = &f.body {
                        let mut builder = ssa::FunctionBuilder::new(
                            sym.clone(),
                            self.mach.clone(),
                            tys.clone(),
                            targ,
                            ty.clone(),
                        );
                        for local in &body.locals {
                            builder.push_local(local.clone());
                        }
                        for block in &body.blocks {
                            builder.push_incoming(block.target, &block.incoming_stack);
                        }
                        for block in &body.blocks {
                            let block_builder =
                                builder.new_basic_block(block.target, &block.incoming_stack);
                            for expr in &block.expr {
                                block_builder.write_expr(expr);
                            }
                            block_builder.write_terminator(&block.term);
                        }

                        Some(builder)
                    } else {
                        None
                    };

                    self.functions.push((
                        sym,
                        FunctionDef {
                            section,
                            linkage,
                            fnty: ty,
                            body,
                        },
                    ));
                }
                ir::MemberDeclaration::Static(_) => todo!("static"),
                ir::MemberDeclaration::OpaqueAggregate(_)
                | ir::MemberDeclaration::AggregateDefinition(_)
                | ir::MemberDeclaration::Empty => {}
            }
        }

        for (name, def) in &self.functions {
            print!("{}{} {}{}", def.section, def.linkage, name, def.fnty);
            if let Some(body) = &def.body {
                println!("{}", body);
            } else {
                println!(";");
            }
        }

        xlang::abi::result::Ok(())
    }

    fn set_target(&mut self, targ: &'static xlang::targets::properties::TargetProperties<'static>) {
        Rc::get_mut(&mut self.mach).unwrap().init_from_target(targ);
        self.targ = Some(targ);
    }
}

impl<M: Machine> XLangCodegen for SsaCodegenPlugin<M> {
    fn target_matches(&self, x: xlang::abi::string::StringView) -> bool {
        self.mach.matches_target(x)
    }

    fn write_output(
        &mut self,
        x: xlang::prelude::v1::DynMut<dyn xlang::abi::io::Write>,
        mode: xlang::plugin::OutputMode,
    ) -> xlang::abi::io::Result<()> {
        let targ = self.targ.expect("set_target must have been called first");
        if mode == xlang::plugin::OutputMode::Obj {
            let mut writer = WriteAdapter::new(x);
            let fmt =
                binfmt::format_by_name(&targ.link.obj_binfmt).expect("obj_binfmt is not supported");

            let mut output = fmt.create_file(binfmt::fmt::FileType::Relocatable);

            let mut sections = vec![
                Section {
                    name: format!(".text"),
                    align: 1024,
                    ty: binfmt::fmt::SectionType::ProgBits,
                    flags: Some(SectionFlag::Alloc | SectionFlag::Executable),
                    ..Section::default()
                },
                Section {
                    name: format!(".rodata"),
                    align: 1024,
                    ty: binfmt::fmt::SectionType::ProgBits,
                    flags: Some(SectionFlag::Alloc.into()),
                    ..Section::default()
                },
                Section {
                    name: format!(".data"),
                    align: 1024,
                    ty: binfmt::fmt::SectionType::ProgBits,
                    flags: Some(SectionFlag::Alloc | SectionFlag::Writable),
                    ..Section::default()
                },
                Section {
                    name: format!(".bss"),
                    align: 1024,
                    ty: binfmt::fmt::SectionType::NoBits,
                    flags: Some(SectionFlag::Alloc | SectionFlag::Writable),
                    ..Section::default()
                },
            ];
            let mut syms = vec![];

            for (sym_name, def) in core::mem::take(&mut self.functions) {
                let sym_kind = match def.linkage {
                    Linkage::External => SymbolKind::Global,
                    Linkage::Internal => SymbolKind::Local,
                    Linkage::Constant => SymbolKind::Local,
                    Linkage::Weak => SymbolKind::Weak,
                };

                if let Some(mut body) = def.body {
                    let section = match def.section {
                        SectionSpec::Global => 0,
                    };
                    let offset = sections[section as usize].offset();
                    let sym_idx = syms.len();
                    let sym = Symbol::new(
                        sym_name,
                        section,
                        offset as u128,
                        SymbolType::Function,
                        sym_kind,
                    );
                    syms.push(sym);

                    try_!(body
                        .write(&mut sections[section as usize], |name, offset| {
                            let inner_sym = Symbol::new(
                                name,
                                section,
                                offset,
                                SymbolType::Null,
                                SymbolKind::Local,
                            );
                            syms.push(inner_sym);
                        })
                        .map_err(Into::into));

                    *syms[sym_idx].size_mut() =
                        Some((sections[section as usize].offset() - offset) as u64);
                } else if sym_kind == SymbolKind::Weak {
                    syms.push(Symbol::new_undef(sym_name, SymbolType::Function, sym_kind));
                }
            }
            match targ.link.stack_attribute_control {
                StackAttributeControlStyle::NoExec => {}
                StackAttributeControlStyle::CveFactory => {
                    eprintln!("Warning: Codegen for target selected sets Stack as Writable")
                }
                StackAttributeControlStyle::GnuStack => {
                    sections.push(Section {
                        name: format!(".note.GNU-stack"),
                        align: 1024,
                        ty: SectionType::NoBits,
                        flags: Some(SectionFlag::Writable.into()),
                        ..Default::default()
                    });
                }
                ctrl => eprintln!("Warning: Unknown stack attribute control style {:?}", ctrl),
            }
            let mut section_map = vec![];
            for section in sections {
                let new_off = output
                    .add_section(section)
                    .expect("Could not add a section");
                section_map.push(new_off)
            }

            for sym in syms.iter_mut() {
                if let Some(sect) = sym.section_mut() {
                    *sect = section_map[(*sect) as usize];
                }
            }
            output.add_symbols(syms).expect("Could not add symbols");
            try_!(fmt.write_file(&mut writer, &output).map_err(Into::into));
            xlang::abi::result::Ok(())
        } else {
            todo!("asm file")
        }
    }
}
