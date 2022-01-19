use xlang::{
    ir::{PointerType, ScalarType, ScalarTypeHeader, ScalarTypeKind, Type},
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
    (align - (size % align)) % align
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
        Type::Void => None,
        Type::FnType(_) => None,
        Type::Pointer(PointerType { .. }) => Some(align_size(
            ((properties.ptrbits as u64) + 7) >> 3,
            properties.ptralign as u64,
        )),
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
        Type::Void | Type::FnType(_) => None,
    }
}
