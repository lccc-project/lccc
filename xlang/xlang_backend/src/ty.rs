use xlang::{
    ir::{ArrayType, PointerType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type, Value},
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

/// Computes the size of the type given by `ty`, according to the `properties` of the current target
pub fn type_size(ty: &Type, properties: &TargetProperties) -> Option<u64> {
    match ty {
        Type::Scalar(ScalarType {
            header:
                ScalarTypeHeader {
                    bitsize,
                    vectorsize,
                    ..
                },
            ..
        }) => {
            let elem_bytes = ((*bitsize as u64) + 7) >> 3;

            let size = if *vectorsize == 0 {
                elem_bytes
            } else {
                elem_bytes * (*vectorsize as u64)
            };

            let align = scalar_align(size, properties);
            Some(align_size(size, align))
        }
        Type::Void | Type::FnType(_) | Type::Null => None,
        Type::Pointer(PointerType { .. }) => Some(align_size(
            ((properties.ptrbits as u64) + 7) >> 3,
            properties.ptralign as u64,
        )),
        Type::Array(ty) => match &**ty {
            ArrayType {
                ty,
                len: Value::Integer { val, .. },
            } => Some(type_size(ty, properties)? * (*val as u64)),
            ArrayType { len, .. } => panic!(
                "Encountered Invalid Value in computation of a type (len={:?})",
                len
            ),
        },
        Type::TaggedType(_, ty) => type_size(ty, properties),
        Type::Product(tys) => {
            let total_align = type_align(ty, properties)?;
            let mut acc_size: u64 = 0;
            for ty in tys {
                let align = type_align(ty, properties)?;
                acc_size = (acc_size.wrapping_neg() & !(align - 1)).wrapping_neg();
                acc_size += type_size(ty, properties)?;
            }
            Some((acc_size.wrapping_neg() & !(total_align - 1)).wrapping_neg())
        }
        Type::Aligned(_, bty) => {
            let align = type_align(ty, properties)?;
            let acc_size = type_size(bty, properties)?;
            Some((acc_size.wrapping_neg() & !(align - 1)).wrapping_neg())
        }
    }
}

/// Computes the size of the type given by `ty`, according to the `properties` of the current target
pub fn type_align(ty: &Type, properties: &TargetProperties) -> Option<u64> {
    match ty {
        Type::Scalar(ScalarType {
            kind: ScalarTypeKind::LongFloat,
            ..
        }) => Some(properties.ldbl_align as u64),
        Type::Scalar(ScalarType {
            header:
                ScalarTypeHeader {
                    bitsize,
                    vectorsize,
                    ..
                },
            ..
        }) => {
            let elem_bytes = ((*bitsize as u64) + 7) >> 3;

            let size = if *vectorsize == 0 {
                elem_bytes
            } else {
                elem_bytes * (*vectorsize as u64)
            };

            Some(scalar_align(size, properties))
        }
        Type::Pointer(_) => Some(properties.ptralign as u64),
        Type::Void | Type::FnType(_) | Type::Null => None,
        Type::Array(ty) => {
            let ArrayType { ty, .. } = &**ty;
            type_align(ty, properties)
        }
        Type::TaggedType(_, ty) => type_align(ty, properties),
        Type::Product(tys) => tys
            .iter()
            .map(|ty| type_align(ty, properties))
            .fold(Some(1), |a, b| a.zip(b).map(|(a, b)| a.max(b))),
        Type::Aligned(val, ty) => {
            let base_align = type_align(ty, properties)?;
            match &**val {
                Value::Invalid(_)
                | Value::Uninitialized(_)
                | Value::String { .. }
                | Value::ByteString { .. }
                | Value::GlobalAddress { .. } => {
                    panic!("Encountered Invalid Value in Computation of Type")
                }
                Value::GenericParameter(_) => panic!("Encountered Generic Parameter in monomorphic code (codegen cannot resolve generics)"),
                Value::Integer { val, .. } => Some((*val as u64).max(base_align)),
            }
        }
    }
}

/// Gets the type of the field of `ty` with `name`
pub fn field_type(ty: &Type, name: &str) -> Option<Type> {
    match ty {
        Type::Null => None,
        Type::Scalar(_) => None,
        Type::Void => None,
        Type::FnType(_) => None,
        Type::Pointer(_) => None,
        Type::Array(_) => None,
        Type::TaggedType(_, ty) => field_type(ty, name),
        Type::Product(tys) => {
            let id = name.parse::<u32>().ok()?;
            tys.get(id as usize).cloned()
        }
        Type::Aligned(_, ty) => field_type(ty, name),
    }
}
