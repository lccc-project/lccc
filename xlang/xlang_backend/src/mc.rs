use core::convert::TryFrom;
use std::{cell::RefCell, io::Write, rc::Rc};

use arch_ops::traits::InsnWrite;
use binfmt::{
    fmt::{FileType, Section},
    sym::Symbol,
};
use xlang::{
    abi::{
        collection::HashMap, io::WriteAdapter, option::Some as XLangSome, pair::Pair,
        result::Result::Ok as XLangOk, span::Span, string::StringView, try_,
    },
    ir::{AccessClass, BinaryOp, FnType, Linkage, PathComponent, PointerKind, Type, UnaryOp},
    plugin::{OutputMode, XLangCodegen, XLangPlugin},
    targets::properties::TargetProperties,
};

use crate::{
    callconv::CallingConvention,
    expr::{Trap, VStackValue, ValLocation},
    str::StringMap,
    ty::TypeInformation,
    FunctionCodegen, FunctionRawCodegen,
};

/// Basic Queries about Machine Features
pub trait MachineFeatures {
    /// The type of Value Locations
    type Loc: ValLocation;

    /// The type of Value Locations
    type CallConv: CallingConvention<Loc = Self::Loc>;

    /// The maximum integer size (in bits) supported natively (without emulation)
    fn native_int_size(&self) -> u16;
    /// The maximum floating point size (in bits) supported natively, or None if no floating-point support exists
    fn native_float_size(&self) -> Option<u16>;

    /// The maximum Vector size supported natively, in bytes
    fn native_vec_size(&self) -> Option<u64> {
        None
    }

    /// Preferred Vector size of the current codegen, in total bytes
    /// This need not be the same as the [`FunctionRawCodegen::native_vec_size`], for example, if some vector types incur a significant runtime performance penalty
    /// (such as AVX-512)
    fn preferred_vec_size(&self) -> Option<u64> {
        None
    }

    /// Whether or not lock-free atomic ops of some size should issue a call to libatomic for this backend.
    fn lockfree_use_libatomic(&self, size: u64) -> bool;

    /// Whether or not lock-free atomic rmws use libatomic
    fn lockfree_cmpxchg_use_libatomic(&self, size: u64) -> bool;

    /// Whether or not BinaryOp can be implemented directly by the CPU
    fn has_wait_free_compound(&self, op: BinaryOp, size: u64) -> bool;

    /// Whether or not the fecth version of BinaryOp can be implemented directly by the CPU
    fn has_wait_free_compound_fetch(&self, op: BinaryOp, size: u64) -> bool;

    /// Whether or not volatile has any effect on codegen
    fn propagate_side_effects(&self) -> bool {
        false
    }

    /// Whether or not the `sequence` XIR instruction has any effect on codegen
    fn propagate_sequence(&self) -> bool {
        false
    }

    /// Mangle the given path. Note: This has already stripped the `Root` and won't be non-mangled paths
    fn mangle(&self, path: &[PathComponent]) -> String;
}

/// An abstract machine instruction, converted by `xlang_backend` from XIR.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
#[non_exhaustive]
pub enum MCInsn<Loc> {
    /// Performs no-operation
    Null,
    /// Move a value from one location to another
    Mov {
        /// The destination location
        dest: MaybeResolved<Loc>,
        /// The source location
        src: MaybeResolved<Loc>,
    },
    /// Moves an immediate value into a location
    MovImm {
        /// The destination location
        dest: MaybeResolved<Loc>,
        /// The Source Value
        src: u128,
    },
    /// Stores from src into the pointer in `dest_ptr`
    StoreIndirect {
        /// The destination pointer
        dest_ptr: MaybeResolved<Loc>,
        /// The Source value
        src: MaybeResolved<Loc>,
        /// Class
        cl: AccessClass,
    },
    /// Stores of an immediate integer into the pointer in `dest_ptr`
    StoreIndirectImm {
        /// The destination pointer
        dest_ptr: MaybeResolved<Loc>,
        /// The source value
        src: u128,
    },
    /// Emits a trap
    Trap {
        /// The kind of trap
        kind: Trap,
    },
    /// Emits a Barrier (Runtime Fence)
    Barrier(AccessClass),
    /// Performs a binary operation, with an immediate as the rhs
    BinaryOpImm {
        /// The first operand and the destination
        dest: MaybeResolved<Loc>,
        /// The second operand (an immediate)
        val: u128,
        /// The Operation performed
        op: BinaryOp,
    },
    /// Performs a binary operation with unknown values on both sides
    BinaryOp {
        /// The first operand and the destination
        dest: MaybeResolved<Loc>,
        /// The second operand
        src: MaybeResolved<Loc>,
        /// The Operation performed
        op: BinaryOp,
    },
    /// Performs a unary operation
    UnaryOp {
        /// The operand
        dest: MaybeResolved<Loc>,
        /// The operation performed
        op: UnaryOp,
    },
    /// Calls the given String by address
    CallSym(String),
    /// Cleans up the frame and does a return
    Return,
    /// Loads the address of a symbol into the given location
    LoadSym {
        /// The location in which to place the symbol
        loc: MaybeResolved<Loc>,
        /// The symbol to resolve
        sym: String,
    },

    /// A Label
    Label(String),
    /// Unconditional Branch
    UnconditionalBranch(String),
}

impl<Loc> Default for MCInsn<Loc> {
    fn default() -> Self {
        Self::Null
    }
}

/// A location (register) allocated by the backend
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Location {
    id: u32,
    has_addr: bool,
    size: u64,
    align: u64,
}

impl Location {
    /// The id of the `Location`
    pub const fn id(&self) -> u32 {
        self.id
    }

    /// Whether or not the location needs/has an address
    pub const fn has_addr(&self) -> bool {
        self.has_addr
    }

    /// The required size of the location
    pub const fn size(&self) -> u64 {
        self.size
    }

    /// The required alignment for the location, if it has an address
    pub const fn align(&self) -> u64 {
        self.align
    }
}

impl ValLocation for Location {
    fn addressible(&self) -> bool {
        self.has_addr
    }

    fn unassigned(n: usize) -> Self {
        Self {
            id: u32::try_from(n).unwrap(),
            has_addr: false,
            size: 0,
            align: 1,
        }
    }
}

/// A value location that is either unresolved or a resolved machine-specific location (such as a register or stack memory)
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MaybeResolved<Loc> {
    /// An unresolved location that is assigned during output
    Unresolved(Location),
    /// A known, resolved, location
    Resolved(Type, Loc),
}

impl<Loc: ValLocation> ValLocation for MaybeResolved<Loc> {
    fn addressible(&self) -> bool {
        match self {
            Self::Unresolved(loc) => loc.has_addr,
            Self::Resolved(_, loc) => loc.addressible(),
        }
    }

    fn unassigned(n: usize) -> Self {
        Self::Unresolved(Location::unassigned(n))
    }
}

/// An Adaptor to a calling convention that yields [`MaybeResolved`] as the location type
pub struct CallConvAdaptor<C>(C);

impl<C: CallingConvention> CallingConvention for CallConvAdaptor<C> {
    type Loc = MaybeResolved<C::Loc>;

    fn pass_return_place(&self, ty: &Type) -> Option<Self::Loc> {
        self.0
            .pass_return_place(ty)
            .map(|loc| MaybeResolved::Resolved(ty.clone(), loc))
    }

    fn find_param(
        &self,
        fnty: &xlang::ir::FnType,
        real: &xlang::ir::FnType,
        param: u32,
        infn: bool,
    ) -> Self::Loc {
        MaybeResolved::Resolved(
            real.params[usize::try_from(param).unwrap()].clone(),
            self.0.find_param(fnty, real, param, infn),
        )
    }

    fn find_return_val(&self, fnty: &xlang::ir::FnType) -> Self::Loc {
        MaybeResolved::Resolved(fnty.ret.clone(), self.0.find_return_val(fnty))
    }
}

/// Raw codegen for generating MCIR from XIR
pub struct MCFunctionCodegen<F: MachineFeatures> {
    inner: F,
    next_loc_id: u32,
    tys: Rc<TypeInformation>,
    mc_insns: Vec<MCInsn<F::Loc>>,
    callconv: CallConvAdaptor<F::CallConv>,
    strings: Rc<RefCell<StringMap>>,
    fn_name: String,
}

#[allow(unused_variables)]
impl<F: MachineFeatures> FunctionRawCodegen for MCFunctionCodegen<F> {
    type Loc = MaybeResolved<F::Loc>;

    type CallConv = CallConvAdaptor<F::CallConv>;

    fn write_trap(&mut self, trap: crate::expr::Trap) {
        self.mc_insns.push(MCInsn::Trap { kind: trap });
    }

    fn write_barrier(&mut self, acc: xlang::ir::AccessClass) {
        self.mc_insns.push(MCInsn::Barrier(acc));
    }

    fn write_int_binary_imm(&mut self, a: Self::Loc, b: u128, _: &Type, op: xlang::ir::BinaryOp) {
        self.mc_insns.push(MCInsn::BinaryOpImm {
            dest: a,
            val: b,
            op,
        });
    }

    fn write_int_binary(&mut self, a: Self::Loc, b: Self::Loc, _: &Type, op: xlang::ir::BinaryOp) {
        self.mc_insns.push(MCInsn::BinaryOp {
            dest: a,
            src: b,
            op,
        });
    }

    fn write_unary(&mut self, val: Self::Loc, _: &Type, op: xlang::ir::UnaryOp) {
        self.mc_insns.push(MCInsn::UnaryOp { dest: val, op });
    }

    fn move_val(&mut self, src: Self::Loc, dest: Self::Loc) {
        self.mc_insns.push(MCInsn::Mov { dest, src });
    }

    fn move_imm(&mut self, src: u128, dest: Self::Loc, ty: &Type) {
        self.mc_insns.push(MCInsn::MovImm { dest, src });
    }

    fn store_indirect_imm(&mut self, src: xlang::ir::Value, ptr: Self::Loc) {
        match src {
            xlang::ir::Value::Integer { ty, val } => self.mc_insns.push(MCInsn::StoreIndirectImm {
                dest_ptr: ptr,
                src: val,
            }),
            _ => todo!(),
        }
    }

    fn load_val(&mut self, lvalue: Self::Loc, loc: Self::Loc) {
        todo!()
    }

    fn store_indirect(&mut self, lvalue: Self::Loc, loc: Self::Loc, ty: &Type) {
        todo!()
    }

    fn get_callconv(&self) -> &Self::CallConv {
        &self.callconv
    }

    fn native_int_size(&self) -> u16 {
        self.inner.native_int_size()
    }

    fn native_float_size(&self) -> xlang::abi::option::Option<u16> {
        self.inner.native_float_size().into()
    }

    fn write_intrinsic(
        &mut self,
        name: xlang::abi::string::StringView,
        params: xlang::vec::Vec<crate::expr::VStackValue<Self::Loc>>,
    ) -> crate::expr::VStackValue<Self::Loc> {
        todo!()
    }

    fn write_target(&mut self, target: u32) {
        let targ = format!("{}._T{}", self.fn_name, target);
        self.mc_insns.push(MCInsn::Label(targ));
    }

    fn call_direct(&mut self, path: &xlang::ir::Path, realty: &xlang::ir::FnType) {
        let addr = match &*path.components {
            [PathComponent::Text(a)] | [PathComponent::Root, PathComponent::Text(a)] => {
                a.clone().to_string()
            }
            [PathComponent::Root, rest @ ..] | [rest @ ..] => self.inner.mangle(rest),
        };
        self.mc_insns.push(MCInsn::CallSym(addr))
    }

    fn call_indirect(&mut self, value: Self::Loc) {
        todo!()
    }

    fn call_absolute(&mut self, addr: u128, realty: &xlang::ir::FnType) {
        todo!()
    }

    fn tailcall_direct(
        &mut self,
        value: xlang::abi::string::StringView,
        ty: &xlang::ir::FnType,
        params: xlang::vec::Vec<crate::expr::VStackValue<Self::Loc>>,
    ) {
        todo!()
    }

    fn tailcall_indirect(
        &mut self,
        value: Self::Loc,
        ty: &xlang::ir::FnType,
        params: xlang::vec::Vec<crate::expr::VStackValue<Self::Loc>>,
    ) {
        todo!()
    }

    fn leave_function(&mut self) {
        self.mc_insns.push(MCInsn::Return)
    }

    fn branch(
        &mut self,
        target: u32,
        condition: xlang::ir::BranchCondition,
        val: crate::expr::VStackValue<Self::Loc>,
    ) {
        todo!()
    }

    fn branch_compare(
        &mut self,
        target: u32,
        condition: xlang::ir::BranchCondition,
        v1: crate::expr::VStackValue<Self::Loc>,
        v2: crate::expr::VStackValue<Self::Loc>,
    ) {
        todo!()
    }

    fn branch_unconditional(&mut self, target: u32) {
        let targ = format!("{}._T{}", self.fn_name, target);
        self.mc_insns.push(MCInsn::UnconditionalBranch(targ));
    }

    fn branch_indirect(&mut self, target: Self::Loc) {
        todo!()
    }

    fn compute_global_address(&mut self, path: &xlang::ir::Path, loc: Self::Loc) {
        let sym = match &*path.components {
            [xlang::ir::PathComponent::Root, xlang::ir::PathComponent::Text(text)]
            | [xlang::ir::PathComponent::Text(text)] => text.to_string(),
            [xlang::ir::PathComponent::Root, rest @ ..] | [rest @ ..] => self.inner.mangle(rest),
        };

        self.mc_insns.push(MCInsn::LoadSym { loc, sym });
    }

    fn compute_label_address(&mut self, target: u32, loc: Self::Loc) {
        todo!()
    }

    fn compute_parameter_address(&mut self, param: u32, loc: Self::Loc) {
        todo!()
    }

    fn compute_local_address(&mut self, inloc: Self::Loc, loc: Self::Loc) {
        todo!()
    }

    fn compute_string_address(
        &mut self,
        enc: crate::str::Encoding,
        bytes: xlang::vec::Vec<u8>,
        loc: Self::Loc,
    ) {
        let sym = self
            .strings
            .borrow_mut()
            .get_string_symbol(bytes, enc)
            .to_string();

        self.mc_insns.push(MCInsn::LoadSym { loc, sym });
    }

    fn free(&mut self, loc: Self::Loc) {
        todo!()
    }

    fn clobber(&mut self, loc: Self::Loc) {
        todo!()
    }

    fn allocate(&mut self, ty: &Type, needs_addr: bool) -> Self::Loc {
        let id = self.next_loc_id;
        self.next_loc_id += 1;
        let size = self.tys.type_size(ty).unwrap();
        let align = self.tys.type_align(ty).unwrap();
        MaybeResolved::Unresolved(Location {
            id,
            size,
            align,
            has_addr: needs_addr,
        })
    }

    fn allocate_lvalue(&mut self, needs_addr: bool) -> Self::Loc {
        let size = self.tys.pointer_size(&Type::Void, PointerKind::Default);
        let align = self.tys.pointer_align(&Type::Void, PointerKind::Default);

        let id = self.next_loc_id;
        self.next_loc_id += 1;
        MaybeResolved::Unresolved(Location {
            id,
            size,
            align,
            has_addr: needs_addr,
        })
    }

    fn write_block_entry_point(&mut self, n: u32) {
        todo!()
    }

    fn write_block_exit_point(&mut self, n: u32) {
        todo!()
    }

    fn write_block_exit(&mut self, n: u32) {
        todo!()
    }

    fn prepare_call_frame(&mut self, callty: &xlang::ir::FnType, realty: &xlang::ir::FnType) {}

    fn lockfree_use_libatomic(&mut self, size: u64) -> bool {
        todo!()
    }

    fn lockfree_cmpxchg_use_libatomic(&mut self, size: u64) -> bool {
        todo!()
    }

    fn has_wait_free_compound(&mut self, op: xlang::ir::BinaryOp, size: u64) -> bool {
        todo!()
    }

    fn has_wait_free_compound_fetch(&mut self, op: xlang::ir::BinaryOp, size: u64) -> bool {
        todo!()
    }

    fn compare_exchange(
        &mut self,
        dest: Self::Loc,
        ctrl: Self::Loc,
        val: Self::Loc,
        ty: &Type,
        ord: xlang::ir::AccessClass,
    ) {
        todo!()
    }

    fn weak_compare_exchange(
        &mut self,
        dest: Self::Loc,
        ctrl: Self::Loc,
        val: Self::Loc,
        ty: &Type,
        ord: xlang::ir::AccessClass,
    ) {
        todo!()
    }

    fn write_asm(
        &mut self,
        _: &xlang::ir::AsmExpr,
        _: xlang::vec::Vec<VStackValue<MaybeResolved<<F as MachineFeatures>::Loc>>>,
    ) -> xlang::vec::Vec<Self::Loc> {
        todo!()
    }
}

/// Trait for types that can write [`MCInsn`]s to an [`InsnWrite`]
pub trait MCWriter {
    /// The [`MachineFeatures`] type
    type Features: MachineFeatures;
    /// The type used to indicate what locations were clobbered at what instruction.
    type Clobbers;
    /// Resolves locations in-place for the given machine code
    fn resolve_locations(
        &self,
        insns: &mut [MCInsn<<Self::Features as MachineFeatures>::Loc>],
        callconv: &<Self::Features as MachineFeatures>::CallConv,
    ) -> Self::Clobbers;
    /// Writes the machine code to the given stream
    fn write_machine_code<I: InsnWrite, F: FnMut(String, u64)>(
        &self,
        insns: &[MCInsn<<Self::Features as MachineFeatures>::Loc>],
        clobbers: Self::Clobbers,
        tys: Rc<TypeInformation>,
        out: &mut I,
        sym_accepter: F,
    ) -> std::io::Result<()>;
    /// Obtains the calling convention for the given `realty`
    fn get_call_conv(
        &self,
        realty: &FnType,
        targ: &'static TargetProperties<'static>,
        features: Span<StringView>,
        ty_info: Rc<TypeInformation>,
    ) -> <Self::Features as MachineFeatures>::CallConv;
    /// Obtains the features from the target
    fn get_features(
        &self,
        properties: &'static TargetProperties<'static>,
        features: Span<StringView>,
    ) -> Self::Features;

    /// Checks if the given target name matches
    fn target_matches(&self, name: &str) -> bool;
}

#[allow(dead_code)] // These will be used more properly later
enum SectionSpec {
    GlobalSection,
    UniqueSection,
    Comdat(String),
    Section(String),
}

struct MCFunctionDecl<W: MCWriter> {
    linkage: Linkage,
    body: Option<(SectionSpec, FunctionCodegen<MCFunctionCodegen<W::Features>>)>,
}

/// Backend that generates MCIR from XIR then writes to a binary file
pub struct MCBackend<W: MCWriter> {
    properties: Option<&'static TargetProperties<'static>>,
    feature: Span<'static, StringView<'static>>,
    strings: Rc<RefCell<StringMap>>,
    writer: W,
    functions: HashMap<String, MCFunctionDecl<W>>,
    tys: Option<Rc<TypeInformation>>,
}

impl<W: MCWriter> MCBackend<W> {
    /// Creates a new backend writer
    pub fn new(x: W) -> Self {
        Self {
            properties: None,
            feature: Span::empty(),
            strings: Rc::new(RefCell::new(StringMap::new())),
            writer: x,
            functions: HashMap::new(),
            tys: None,
        }
    }
}

impl<W: MCWriter> XLangPlugin for MCBackend<W> {
    fn accept_ir(
        &mut self,
        ir: &mut xlang::ir::File,
    ) -> xlang::abi::result::Result<(), xlang::plugin::Error> {
        let mut tys = TypeInformation::from_properties(self.properties.unwrap());
        for Pair(path, mem) in &ir.root.members {
            match &mem.member_decl {
                xlang::ir::MemberDeclaration::OpaqueAggregate(_) => {
                    tys.add_opaque_aggregate(path.clone())
                }
                xlang::ir::MemberDeclaration::AggregateDefinition(defn) => {
                    tys.add_aggregate(path.clone(), defn.clone())
                }
                _ => {}
            }
        }
        let tys = Rc::new(tys);
        for Pair(path, mem) in &ir.root.members {
            match &mem.member_decl {
                xlang::ir::MemberDeclaration::Function(f) => {
                    let features = self
                        .writer
                        .get_features(self.properties.unwrap(), self.feature);
                    let mangled_name = match &*path.components {
                        [PathComponent::Root, PathComponent::Text(n)]
                        | [PathComponent::Text(n)] => n.to_string(),
                        [PathComponent::Root, rest @ ..] | [rest @ ..] => features.mangle(rest),
                    };
                    let linkage = f.linkage;
                    let body = if let XLangSome(body) = &f.body {
                        let section_spec = SectionSpec::GlobalSection;

                        let innercg = MCFunctionCodegen {
                            fn_name: mangled_name.clone(),
                            inner: features,
                            next_loc_id: 0,
                            tys: tys.clone(),
                            mc_insns: Vec::new(),
                            strings: self.strings.clone(),
                            callconv: CallConvAdaptor(self.writer.get_call_conv(
                                &f.ty,
                                self.properties.unwrap(),
                                self.feature,
                                Rc::clone(&tys),
                            )),
                        };
                        let mut fncg = FunctionCodegen::new(
                            innercg,
                            path.clone(),
                            f.ty.clone(),
                            self.properties.unwrap(),
                            tys.clone(),
                        );

                        fncg.write_function_body(body);

                        Some((section_spec, fncg))
                    } else {
                        None
                    };

                    self.functions
                        .insert(mangled_name, MCFunctionDecl { linkage, body });
                }
                xlang::ir::MemberDeclaration::Static(_) => todo!("static"),
                _ => {}
            }
        }
        self.tys = Some(tys);
        XLangOk(())
    }

    fn set_target(&mut self, targ: &'static TargetProperties<'static>) {
        self.properties = Some(targ);
    }
}

impl<W: MCWriter> XLangCodegen for MCBackend<W> {
    fn target_matches(&self, x: StringView) -> bool {
        self.writer.target_matches(&x)
    }

    fn write_output(
        &mut self,
        x: xlang::abi::traits::DynMut<dyn xlang::abi::io::Write>,
        mode: xlang::plugin::OutputMode,
    ) -> xlang::abi::io::Result<()> {
        assert!(matches!(mode, OutputMode::Obj));
        let mut syms = Vec::new();
        let props = self.properties.unwrap();
        let binfmt = binfmt::format_by_name(&props.link.obj_binfmt).unwrap();

        let mut binfile = binfmt.create_file(FileType::Relocatable);

        let mut rodata = Section {
            name: String::from(".rodata"),
            align: 1024,
            ty: binfmt::fmt::SectionType::ProgBits,
            ..Default::default()
        };

        let mut text = Section {
            name: String::from(".text"),
            align: 1024,
            ty: binfmt::fmt::SectionType::ProgBits,
            ..Default::default()
        };

        for (enc, name, content) in self.strings.borrow().symbols() {
            let st = Symbol::new(
                name.to_string(),
                Some(0),
                Some(rodata.offset() as u128),
                binfmt::sym::SymbolType::Object,
                binfmt::sym::SymbolKind::Local,
            );
            let bytes = enc.encode_utf8(content);
            try_!(rodata.write_all(&bytes).map_err(Into::into));
            syms.push(st);
        }

        for Pair(name, func) in core::mem::take(&mut self.functions) {
            let symty = match func.linkage {
                Linkage::External => binfmt::sym::SymbolKind::Global,
                Linkage::Internal | Linkage::Constant => binfmt::sym::SymbolKind::Local,
                Linkage::Weak => binfmt::sym::SymbolKind::Weak,
            };

            if let Some((_, func)) = func.body {
                let mut inner = func.into_inner();
                let cc = &inner.callconv;
                let clobbers = self.writer.resolve_locations(&mut inner.mc_insns, &cc.0);

                let sym = Symbol::new(
                    name,
                    Some(1),
                    Some(text.offset() as u128),
                    binfmt::sym::SymbolType::Function,
                    symty,
                );

                try_!(self
                    .writer
                    .write_machine_code(
                        &inner.mc_insns,
                        clobbers,
                        self.tys.clone().unwrap(),
                        &mut text,
                        |label, offset| {
                            syms.push(Symbol::new(
                                label,
                                Some(1),
                                Some(offset as u128),
                                binfmt::sym::SymbolType::Function,
                                binfmt::sym::SymbolKind::Local,
                            ))
                        }
                    )
                    .map_err(Into::into));

                syms.push(sym);
            } else if func.linkage == Linkage::Weak {
                let sym = Symbol::new(name, None, None, binfmt::sym::SymbolType::Function, symty);

                syms.push(sym);
            }
        }

        let rodatano = binfile.add_section(rodata).unwrap();
        let textno = binfile.add_section(text).unwrap();

        for mut sym in syms {
            match sym.section_mut() {
                Some(x @ 0) => *x = rodatano,
                Some(x @ 1) => *x = textno,
                None => {}
                Some(x) => panic!("Unexpected section number for symbol {}", x),
            }

            let insym = binfile.get_or_create_symbol(sym.name()).unwrap();

            if sym.value().is_some() || insym.value().is_none() {
                *insym = sym;
            }
        }

        let mut adaptor = WriteAdapter::new(x);

        try_!(binfmt
            .write_file(&mut adaptor, &binfile)
            .map_err(Into::into));

        XLangOk(())
    }
}
