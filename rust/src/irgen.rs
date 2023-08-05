pub mod visitor;

pub mod name_visitor;
pub mod xir_visitor;

use crate::{
    interning::Symbol,
    irgen::xir_visitor::{XirModTypeGatherer, XirModVisitor},
    sema::{DefId, Definitions},
};

use xlang::ir;
use xlang::{abi::collection::HashMap, targets::properties::TargetProperties};

use name_visitor::NameModVisitor;

pub type NameMap = HashMap<DefId, Symbol>;

#[allow(dead_code)]
pub struct IntMangler {
    i8: u8,
    i16: u8,
    i32: u8,
    i64: u8,
    i128: u8,
    u8: u8,
    u16: u8,
    u32: u8,
    u64: u8,
    u128: u8,
}

impl IntMangler {
    fn new(properties: &TargetProperties) -> Self {
        let properties = properties.primitives;
        let x32 = if properties.intbits == 32 {
            b'i'
        } else if properties.longbits == 32 {
            b'l'
        } else {
            panic!("no 32-bit integer type found");
        };
        let x64 = if properties.longbits == 64 {
            b'l'
        } else if properties.llongbits == 64 {
            b'x'
        } else {
            panic!("no 64-bit integer type found");
        };
        Self {
            i8: b'a',
            i16: b's',
            i32: x32,
            i64: x64,
            i128: b'n',
            u8: b'h',
            u16: b't',
            u32: x32 + 1,
            u64: x64 + 1,
            u128: b'o',
        }
    }
}

pub fn irgen(defs: &mut Definitions, file: &mut ir::File, properties: &TargetProperties) {
    let int_mangler = IntMangler::new(properties);
    let mut names = NameMap::new();
    defs.visit_all_crates(NameModVisitor::new(&mut names, &int_mangler));
    println!("{:?}\n", names);
    let mut tys = HashMap::new();
    defs.visit_all_crates(XirModTypeGatherer::new(defs, &names, &mut tys, properties));
    println!("{:?}\n", tys);
    defs.visit_all_crates(XirModVisitor::new(defs, &names, &tys, file, properties));
}
