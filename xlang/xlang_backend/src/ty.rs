use std::convert::TryInto;

use xlang::{
    ir::{
        AggregateDefinition, AggregateKind, AnnotationItem, Path, PointerKind, ScalarType,
        ScalarTypeHeader, ScalarTypeKind, Type, Value,
    },
    prelude::v1::{HashMap, Pair, Some as XLangSome},
    targets::properties::TargetProperties,
};

fn scalar_align(size: u64, max_align: u16) -> u64 {
    if size <= (max_align as u64) {
        size.next_power_of_two()
    } else {
        max_align as u64
    }
}

fn align_size(size: u64, align: u64) -> u64 {
    (size.wrapping_neg() & !(align - 1)).wrapping_neg()
}

/// The layout of a particular aggregate type
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AggregateLayout {
    total_size: u64,
    total_align: u64,
    fields: HashMap<String, (u64, Type)>,
    transparent_over: Option<Type>,
}

///
/// A map of information about the types on the system
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct TypeInformation {
    aggregates: HashMap<Path, Option<AggregateDefinition>>,
    aliases: HashMap<Path, Type>,
    properties: &'static TargetProperties,
}

impl TypeInformation {
    /// Constructs a new set of [`TypeInformation`] from the properties of a given target
    pub fn from_properties(properties: &'static TargetProperties) -> Self {
        Self {
            properties,
            aliases: HashMap::new(),
            aggregates: HashMap::new(),
        }
    }

    /// Adds a new aggregate type at the given path
    pub fn add_aggregate(&mut self, path: Path, defn: AggregateDefinition) {
        self.aggregates.insert(path, Some(defn));
    }

    /// Adds a new opaque type at the given path
    pub fn add_opaque_aggregate(&mut self, path: Path) {
        self.aggregates.get_or_insert_mut(path, None);
    }

    /// Computes the layout of an aggregate type from it's definition
    pub fn aggregate_layout_from_defn(&self, defn: &AggregateDefinition) -> AggregateLayout {
        let mut align = 1u64;
        let mut size = 0u64;
        let mut fields = HashMap::new();
        let mut transparent_over = None;
        let mut is_transparent = false;

        let mut sort_align = false;

        for attr in &defn.annotations.annotations {
            match &*attr.items {
                [AnnotationItem::Meta(m, an)] => {
                    if *m == xlang::ir::simple_path!(sort_layout) {
                        match &*an.items {
                            [AnnotationItem::Identifier(id)]
                                if *id == xlang::ir::simple_path!(alignment) =>
                            {
                                sort_align = true;
                            }
                            val @ [..] => panic!("Cannot sort by {:?}", val),
                        }
                    }
                }
                [AnnotationItem::Identifier(id)] if *id == xlang::ir::simple_path!(transparent) => {
                    is_transparent = true;
                }
                _ => {}
            }
        }

        let mut base_fields = defn.fields.clone();

        if sort_align && defn.kind != AggregateKind::Union {
            base_fields.sort_by_key(|Pair(_, ty)| self.type_align(ty).unwrap());
        }

        for Pair(name, ty) in base_fields {
            let name = name.to_string();
            let falign = self.type_align(&ty).unwrap();
            align = falign.max(align);
            let fsize = self.type_size(&ty).unwrap();

            if (falign != 1 || fsize != 0) && is_transparent {
                if core::mem::replace(&mut transparent_over, Some(ty.clone())).is_some() {
                    panic!("Cannot be transparent over multiple non- 1-ZST fields")
                }
            }
            match defn.kind {
                AggregateKind::Struct => {
                    size = align_size(size, falign);
                    let offset = size;
                    size += fsize;
                    fields.insert(name, (offset, ty));
                }
                AggregateKind::Union => {
                    size = fsize.max(size);
                    fields.insert(name, (0, ty));
                }
            }
        }

        AggregateLayout {
            total_size: size,
            total_align: align,
            fields,
            transparent_over,
        }
    }

    /// Determines the total aggregate layout of a type
    pub fn aggregate_layout(&self, ty: &Type) -> Option<AggregateLayout> {
        match ty {
            Type::TaggedType(_, ty) => self.aggregate_layout(ty),
            Type::Aligned(_, ty) => self.aggregate_layout(ty),
            Type::Product(elems) => {
                let mut elems = elems.clone();
                elems.sort_by_key(|ty| self.type_align(ty).unwrap());

                let mut align = 1u64;
                let mut size = 0u64;
                let mut fields = HashMap::new();

                for (i, field) in elems.into_iter().enumerate() {
                    align = self.type_align(&field)?.max(align);
                    let offset = size;
                    size += self.type_size(&field)?;
                    fields.insert(i.to_string(), (offset, field));
                }

                Some(AggregateLayout {
                    total_size: size,
                    total_align: align,
                    fields,
                    transparent_over: None,
                })
            }
            Type::Aggregate(defn) => Some(self.aggregate_layout_from_defn(defn)),
            Type::Named(_) => todo!(),
            _ => None,
        }
    }

    /// Computes the alignment of a type
    pub fn type_align(&self, ty: &Type) -> Option<u64> {
        match ty {
            Type::Null | Type::Void | Type::FnType(_) => None,
            Type::Scalar(ScalarType {
                kind: ScalarTypeKind::LongFloat,
                ..
            }) => Some(self.properties.ldbl_align.into()),
            Type::Scalar(ScalarType {
                header: ScalarTypeHeader { bitsize, .. },
                ..
            }) => {
                let raw_size = (((*bitsize) + 7) / 8).into();
                Some(scalar_align(raw_size, self.properties.max_align))
            }
            Type::Pointer(pty) => {
                let raw_size = match pty.kind {
                    PointerKind::Default => {
                        if let Type::FnType(_) = &*pty.inner {
                            self.properties.fnptrbits
                        } else {
                            self.properties.ptrbits
                        }
                    }
                    PointerKind::Near => self.properties.nearptrbits,
                    PointerKind::Far => self.properties.farptrbits,
                    kind => panic!("Invalid pointer kind {:?}", kind),
                }
                .into();

                Some(scalar_align(raw_size, self.properties.ptralign))
            }
            Type::Array(arrty) => self.type_align(&arrty.ty),
            Type::TaggedType(_, ty) => self.type_align(ty),
            ty => self.aggregate_layout(ty).map(|layout| layout.total_align),
        }
    }

    /// Computes the size of a type
    pub fn type_size(&self, ty: &Type) -> Option<u64> {
        match ty {
            Type::Null | Type::Void | Type::FnType(_) => None,
            Type::Scalar(ScalarType {
                header:
                    ScalarTypeHeader {
                        bitsize,
                        vectorsize,
                        ..
                    },
                ..
            }) => {
                let raw_size = (((*bitsize) + 7) / 8).into();
                let aligned_size =
                    align_size(raw_size, scalar_align(raw_size, self.properties.max_align));
                if let XLangSome(v) = vectorsize {
                    let vsize = aligned_size * u64::from(*v);
                    Some(vsize)
                } else {
                    Some(aligned_size)
                }
            }
            Type::Pointer(ty) => {
                let raw_size = match ty.kind {
                    PointerKind::Default => {
                        if let Type::FnType(_) = &*ty.inner {
                            self.properties.fnptrbits
                        } else {
                            self.properties.ptrbits
                        }
                    }
                    PointerKind::Near => self.properties.nearptrbits,
                    PointerKind::Far => self.properties.farptrbits,
                    kind => panic!("Invalid pointer kind {:?}", kind),
                }
                .into();

                Some(align_size(
                    raw_size,
                    scalar_align(raw_size, self.properties.ptralign),
                ))
            }
            Type::Array(a) => {
                let size = self.type_size(&a.ty)?;
                match &a.len {
                    Value::Invalid(_) | Value::Uninitialized(_) => None,
                    Value::Integer {
                        ty:
                            ScalarType {
                                kind: ScalarTypeKind::Integer { .. },
                                ..
                            },
                        val,
                    } => size.checked_mul((*val).try_into().ok()?),
                    v => panic!("Encountered invalid value in computation of a type {:?}", v),
                }
            }
            Type::TaggedType(_, ty) => self.type_size(ty),
            ty => self.aggregate_layout(ty).map(|layout| layout.total_size),
        }
    }
}
