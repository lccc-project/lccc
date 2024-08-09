use xlang::{
    ir::{
        AggregateDefinition, AggregateKind, AnnotationItem, Path, PointerAliasingRule, PointerKind,
        ScalarType, ScalarTypeHeader, ScalarTypeKind, ScalarValidity, Type, Value,
    },
    prelude::v1::{HashMap, Some as XLangSome},
    targets::properties::TargetProperties,
};

use crate::expr::{LValue, NoOpaque, VStackValue};

use core::cell::RefCell;

pub(crate) fn scalar_align(size: u64, max_align: u16) -> u64 {
    if size <= (max_align as u64) {
        size.next_power_of_two()
    } else {
        max_align as u64
    }
}

pub(crate) fn align_size(size: u64, align: u64) -> u64 {
    (size.wrapping_neg() & !(align - 1)).wrapping_neg()
}

/// The layout of a particular aggregate type
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct AggregateLayout {
    /// The total size of the aggregate type
    pub total_size: u64,
    /// The alignment of the aggregate type
    pub total_align: u64,
    /// A map between field names and the field offset/type
    pub fields: HashMap<String, (u64, Type)>,
    /// Set to the type the aggregate is transparent over, if any
    pub transparent_over: Option<Type>,
    /// Set to the first niche this type has and the new type that fills it, if any such niches are available
    pub first_niche: Option<(Type, VStackValue<NoOpaque>)>,
}

///
/// A map of information about the types on the system
#[derive(Clone, Debug)]
pub struct TypeInformation {
    aggregates: HashMap<Path, Option<AggregateDefinition>>,
    aliases: HashMap<Path, Type>,
    aggregate_layout_cache: RefCell<HashMap<Type, Box<AggregateLayout>>>,
    properties: &'static TargetProperties<'static>,
}

impl TypeInformation {
    /// Constructs a new set of [`TypeInformation`] from the properties of a given target
    pub fn from_properties(properties: &'static TargetProperties) -> Self {
        Self {
            properties,
            aliases: HashMap::new(),
            aggregates: HashMap::new(),
            aggregate_layout_cache: RefCell::new(HashMap::new()),
        }
    }

    /// Computes the first niche value of `ty`, returning the replacement type and the value of that type that fills that niche
    pub fn first_niche_for(&self, ty: &Type) -> Option<(Type, VStackValue<NoOpaque>)> {
        match ty {
            Type::Null => None,
            Type::Scalar(sty) => match *sty {
                ScalarType { mut header, kind }
                    if header.validity.contains(ScalarValidity::NONZERO) =>
                {
                    header.validity -= ScalarValidity::NONZERO;
                    Some((
                        Type::Scalar(ScalarType { header, kind }),
                        VStackValue::Constant(Value::Integer {
                            ty: ScalarType { header, kind },
                            val: 0,
                        }),
                    ))
                }
                ScalarType {
                    header,
                    kind:
                        ScalarTypeKind::Integer {
                            signed,
                            min,
                            max: XLangSome(max),
                        },
                } => {
                    let next = (max as u128) + 1;
                    let ty_max = u128::MAX
                        >> (128u16
                            .saturating_sub(header.bitsize)
                            .saturating_add(signed as u16) as u32);

                    if (((max as u128) >= ty_max) && !signed)
                        || ((max >= (ty_max as i128)) && signed)
                    {
                        None
                    } else {
                        let sty = ScalarType {
                            header,
                            kind: ScalarTypeKind::Integer {
                                signed,
                                min,
                                max: XLangSome(next as i128),
                            },
                        };
                        Some((
                            Type::Scalar(sty),
                            VStackValue::Constant(Value::Integer { ty: sty, val: next }),
                        ))
                    }
                }
                _ => None,
            },
            Type::Void => todo!(),
            Type::FnType(_) => todo!(),
            Type::Pointer(_) => todo!(),
            Type::Array(_) => todo!(),
            Type::TaggedType(_, _) => todo!(),
            Type::Product(_) => todo!(),
            Type::Aligned(_, _) => todo!(),
            Type::Aggregate(_) => todo!(),
            Type::Named(_) => todo!(),
        }
    }

    /// Produces the zero-init value of `ty`
    pub fn zero_init(&self, ty: &Type) -> Option<VStackValue<NoOpaque>> {
        match ty {
            Type::Null => None,
            Type::Scalar(sty) => {
                if sty.header.validity.contains(ScalarValidity::NONZERO) {
                    Some(VStackValue::Constant(Value::Invalid(Type::Scalar(*sty))))
                } else {
                    Some(VStackValue::Constant(Value::Integer { ty: *sty, val: 0 }))
                }
            }
            Type::Void => None,
            Type::FnType(_) => None,
            Type::Pointer(pty) => {
                if pty.alias.contains(PointerAliasingRule::NONNULL) || pty.valid_range.1 > 0 {
                    Some(VStackValue::Constant(Value::Invalid(Type::Pointer(
                        pty.clone(),
                    ))))
                } else {
                    Some(VStackValue::Pointer(pty.clone(), LValue::Null))
                }
            }
            Type::Array(arrty) => {
                let repeat = arrty.len.clone();
                let val = self.zero_init(&arrty.ty)?;
                Some(VStackValue::ArrayRepeat(Box::new(val), repeat))
            }
            Type::TaggedType(_, ty) => self.zero_init(ty),
            Type::Product(prod) => {
                let fields = prod
                    .iter()
                    .enumerate()
                    .map(|(field, b)| Some((format!("{}", field), self.zero_init(b)?)))
                    .collect::<Option<_>>()?;
                Some(VStackValue::AggregatePieced(
                    Type::Product(prod.clone()),
                    fields,
                ))
            }
            Type::Aligned(_, _) => todo!(),
            Type::Aggregate(_) => todo!(),
            Type::Named(_) => todo!(),
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
    fn aggregate_layout_from_defn(&self, defn: &AggregateDefinition) -> AggregateLayout {
        let mut align = 1u64;
        let mut size = 0u64;
        let mut fields = HashMap::new();
        let mut transparent_over = None;
        let mut is_transparent = false;

        let mut sort_align = false;

        for attr in &defn.annotations.annotations {
            match &attr.inner {
                AnnotationItem::Meta(m, an) => {
                    if *m == xlang::ir::simple_path!(sort_layout) {
                        match &an[..] {
                            [AnnotationItem::Identifier(id)]
                                if *id == xlang::ir::simple_path!(alignment) =>
                            {
                                sort_align = true;
                            }
                            val @ [..] => panic!("Cannot sort by {:?}", val),
                        }
                    }
                }
                AnnotationItem::Identifier(id) if *id == xlang::ir::simple_path!(transparent) => {
                    is_transparent = true;
                }
                _ => {}
            }
        }

        let mut base_fields = defn.fields.clone();

        if sort_align && defn.kind != AggregateKind::Union {
            base_fields.sort_by_key(|field| self.type_align(&field.ty).unwrap());
        }

        for field in base_fields {
            let name = field.name.to_string();
            let falign = self.type_align(&field.ty).unwrap();
            align = falign.max(align);
            let fsize = self.type_size(&field.ty).unwrap();

            assert!(
                !((falign != 1 || fsize != 0)
                    && is_transparent
                    && core::mem::replace(&mut transparent_over, Some(field.ty.clone())).is_some()),
                "Cannot be transparent over multiple non- 1-ZST fields"
            );
            match defn.kind {
                AggregateKind::Struct => {
                    size = align_size(size, falign);
                    let offset = size;
                    size += fsize;
                    fields.insert(name, (offset, field.ty));
                }
                AggregateKind::Union => {
                    size = fsize.max(size);
                    fields.insert(name, (0, field.ty));
                }
            }
        }

        AggregateLayout {
            total_size: size,
            total_align: align,
            fields,
            transparent_over,
            first_niche: None,
        }
    }

    /// Determines the total aggregate layout of a type
    pub fn aggregate_layout(&self, ty: &Type) -> Option<&AggregateLayout> {
        let cache = self.aggregate_layout_cache.borrow();
        if let Some(layout) = cache.get(ty) {
            unsafe { Some(&*((&**layout) as *const AggregateLayout)) }
        } else {
            drop(cache);
            let layout = match ty {
                Type::TaggedType(_, ty) => return self.aggregate_layout(ty),
                Type::Aligned(_, ty) => return self.aggregate_layout(ty),
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
                        first_niche: None,
                    })
                }
                Type::Aggregate(defn) => Some(self.aggregate_layout_from_defn(defn)),
                Type::Named(p) => {
                    if let Some(ty) = self.aliases.get(p) {
                        return self.aggregate_layout(ty);
                    } else if let Some(Some(defn)) = self.aggregates.get(p) {
                        Some(self.aggregate_layout_from_defn(defn))
                    } else {
                        None
                    }
                }
                _ => None,
            };

            if let Some(layout) = layout {
                let mut cache = self.aggregate_layout_cache.borrow_mut();
                cache.insert(ty.clone(), Box::new(layout));
                unsafe { Some(&*((&*cache[ty]) as *const AggregateLayout)) }
            } else {
                None
            }
        }
    }

    /// Computes the alignment of a type
    pub fn type_align(&self, ty: &Type) -> Option<u64> {
        match ty {
            Type::Null | Type::Void | Type::FnType(_) => None,
            Type::Scalar(ScalarType {
                header: ScalarTypeHeader { bitsize, .. },
                ..
            }) => {
                let raw_size = (((*bitsize) + 7) / 8).into();
                Some(scalar_align(raw_size, self.properties.primitives.max_align))
            }
            Type::Pointer(pty) => {
                let raw_size = u64::from(match pty.kind {
                    PointerKind::Default => {
                        if let Type::FnType(_) = &*pty.inner {
                            self.properties.primitives.fnptrbits
                        } else {
                            self.properties.primitives.ptrbits
                        }
                    }
                    PointerKind::Near => self.properties.primitives.nearptrbits,
                    PointerKind::Far => self.properties.primitives.farptrbits,
                    kind => panic!("Invalid pointer kind {:?}", kind),
                }) / 8;

                Some(scalar_align(raw_size, self.properties.primitives.ptralign))
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
                let aligned_size = align_size(
                    raw_size,
                    scalar_align(raw_size, self.properties.primitives.max_align),
                );
                if let XLangSome(v) = vectorsize {
                    let vsize = aligned_size * u64::from(*v);
                    Some(vsize)
                } else {
                    Some(aligned_size)
                }
            }
            Type::Pointer(ty) => {
                let raw_size = u64::from(match ty.kind {
                    PointerKind::Default => {
                        if let Type::FnType(_) = &*ty.inner {
                            self.properties.primitives.fnptrbits
                        } else {
                            self.properties.primitives.ptrbits
                        }
                    }
                    PointerKind::Near => self.properties.primitives.nearptrbits,
                    PointerKind::Far => self.properties.primitives.farptrbits,
                    kind => panic!("Invalid pointer kind {:?}", kind),
                }) / 8;

                Some(align_size(
                    raw_size,
                    scalar_align(raw_size, self.properties.primitives.ptralign),
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

    /// Gets the type of the field of `ty` with a given name
    pub fn get_field_type(&self, ty: &Type, name: &str) -> Option<Type> {
        self.aggregate_layout(ty)
            .map(|layout| &layout.fields)
            .map(|fields| fields.get(name).cloned().map(|(_, ty)| ty))
            .and_then(Into::into)
    }

    /// The size of a pointer type. May be more efficient to use over `type_size` if you don't already have a pointer type
    pub fn pointer_size(&self, pointee: &Type, kind: PointerKind) -> u64 {
        let rawwidth = match kind {
            PointerKind::Far => self.properties.primitives.farptrbits,
            PointerKind::Near => self.properties.primitives.nearptrbits,
            PointerKind::Default => match pointee {
                Type::FnType(_) => self.properties.primitives.fnptrbits,
                _ => self.properties.primitives.ptrbits,
            },
            kind => panic!("Unexpected pointer kind {:?}", kind),
        };
        let rawsize = ((rawwidth + 7) / 8) as u64;
        let maxalign = self.properties.primitives.ptralign as u64;
        if rawsize.next_power_of_two() <= maxalign {
            rawsize.next_power_of_two()
        } else {
            (rawsize + (maxalign - 1)) & (!(maxalign - 1))
        }
    }

    /// The alignment requirement of a pointer type. May be more efficient to use over `type_align` if you don't already have a pointer type
    pub fn pointer_align(&self, pointee: &Type, kind: PointerKind) -> u64 {
        let size = self.pointer_size(pointee, kind);
        let maxalign = self.properties.primitives.ptralign as u64;
        size.min(maxalign)
    }

    /// Checks if the given type can have atomic operations performed natively, w/o a global mutex
    pub fn atomic_is_lock_free(&self, ty: &Type) -> Option<bool> {
        let tysize = self.type_size(ty)?;

        let target_lockfree_mask = self.properties.primitives.lock_free_atomic_mask
            | self.properties.arch.lock_free_atomic_masks;

        if tysize == 0 {
            Some(false)
        } else {
            let next_pow_of_two = tysize.next_power_of_two();
            let bit = 64 - (next_pow_of_two.leading_zeros());

            if bit > 16 {
                Some(false)
            } else {
                Some((((target_lockfree_mask) & (1u16 << (bit - 1))) >> (bit - 1)) != 0)
            }
        }
    }

    /// Checks the required alignment for atomic operations of a particular size
    pub fn atomic_required_alignment(&self, ty: &Type) -> Option<u64> {
        let tysize = self.type_size(ty)?;

        Some(scalar_align(
            tysize,
            self.properties.primitives.max_atomic_align,
        ))
    }
}
