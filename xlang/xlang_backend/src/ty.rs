use std::convert::TryInto;

use xlang::{
    ir::{
        AggregateDefinition, AggregateKind, Path, ScalarType, ScalarTypeHeader, ScalarTypeKind,
        Type, Value,
    },
    prelude::v1::{HashMap, Pair, Some as XLangSome},
    targets::properties::TargetProperties,
};

fn scalar_align(size: u64, properties: &TargetProperties) -> u64 {
    if size <= (properties.max_align as u64) {
        size.next_power_of_two()
    } else {
        properties.max_align as u64
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

    /// Computes the alignment of a type
    pub fn type_align(&self, _ty: &Type) -> Option<u64> {
        todo!()
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
                let aligned_size = align_size(raw_size, scalar_align(raw_size, self.properties));
                if let XLangSome(v) = vectorsize {
                    let vsize = aligned_size * u64::from(*v);
                    Some(align_size(vsize, scalar_align(vsize, self.properties)))
                } else {
                    Some(aligned_size)
                }
            }
            Type::Pointer(_) => {
                let raw_size = ((self.properties.ptrbits + 7) / 8).into();

                Some(align_size(raw_size, self.properties.ptralign.into()))
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
            Type::Product(tys) => {
                let mut tys = tys.clone();
                tys.sort_by(|a, b| self.type_align(a).cmp(&self.type_align(b)));
                let raw_size =
                    tys.iter()
                        .map(|s| self.type_size(s))
                        .fold(Some(0u64), |a, b| match (a, b) {
                            (None, _) | (_, None) => None,
                            (Some(x), Some(y)) => x.checked_add(y),
                        })?;

                let align =
                    tys.iter()
                        .map(|s| self.type_align(s))
                        .fold(Some(1u64), |a, b| match (a, b) {
                            (None, _) | (_, None) => None,
                            (Some(x), Some(y)) => Some(x.max(y)),
                        })?;
                Some(align_size(raw_size, align))
            }
            Type::Aligned(alignment, ty) => {
                let raw_size = self.type_size(ty)?;

                let alignment = match &**alignment {
                    Value::Invalid(_) | Value::Uninitialized(_) => None,
                    Value::Integer {
                        ty:
                            ScalarType {
                                kind: ScalarTypeKind::Integer { .. },
                                ..
                            },
                        val,
                    } => (*val).try_into().ok(),
                    v => panic!("Invalid value encountered in type {:?}", v),
                }?;
                Some(align_size(raw_size, alignment))
            }
            Type::Aggregate(a) => {
                let align = a
                    .fields
                    .iter()
                    .map(|Pair(_, ty)| ty)
                    .map(|s| self.type_align(s))
                    .fold(Some(1u64), |a, b| match (a, b) {
                        (None, _) | (_, None) => None,
                        (Some(x), Some(y)) => Some(x.max(y)),
                    })?;
                match a.kind {
                    AggregateKind::Struct => {
                        let raw_size = a
                            .fields
                            .iter()
                            .map(|Pair(_, ty)| ty)
                            .map(|ty| self.type_size(ty))
                            .fold(Some(0u64), |a, b| match (a, b) {
                                (None, _) | (_, None) => None,
                                (Some(x), Some(y)) => x.checked_add(y),
                            })?;

                        Some(align_size(raw_size, align))
                    }
                    AggregateKind::Union => {
                        let raw_size = a
                            .fields
                            .iter()
                            .map(|Pair(_, ty)| ty)
                            .map(|s| self.type_size(s))
                            .reduce(|a, b| match (a, b) {
                                (None, _) | (_, None) => None,
                                (Some(x), Some(y)) => Some(x.max(y)),
                            })
                            .and_then(|s| s)?;
                        Some(align_size(raw_size, align))
                    }
                }
            }
            Type::Named(path) => todo!("{:?}", path),
        }
    }
}
