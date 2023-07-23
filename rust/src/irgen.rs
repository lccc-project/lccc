
use crate::sema::Definitions;

use xlang::abi::collection::HashMap;
use xlang::ir;
use xlang::targets::{properties::get_properties, Target};

pub mod visitor;
pub mod name_visitor;

use name_visitor::NameModVisitor;

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
    fn new(target: &Target) -> Self {
        let properties = get_properties(target).unwrap().primitives;
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

pub fn irgen(defs: &mut Definitions, file: &mut ir::File) {
    let int_mangler = IntMangler::new(&file.target);
    let mut names = HashMap::new();
    defs.visit_all_crates(NameModVisitor::new(&mut names, &int_mangler));
    println!("{:?}", names);
    todo!()
}
