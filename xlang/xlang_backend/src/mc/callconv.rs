// use xlang::ir::FloatFormat;

// pub enum TypeMode {
//     Null,
//     Memory,
//     Integer,
//     Float(FloatFormat),
//     IntVector,
//     FloatVector,
//     Struct(Vec<(u64, TypeMode)>),
//     Union(Vec<TypeMode>),
//     ComplexFloat(FloatFormat),
// }

// pub trait CallConvTag {
//     type Register: Clone + PartialEq + Hash;

//     type CallMode: Clone + PartialEq + Hash;

//     /// All modes available
//     fn all_modes(&self) -> &[Self::CallMode];

//     /// All registers available for a particular call mode
//     fn param_registers_for_mode(&self, mode: Self::CallMode) -> &[Self::Register];

//     /// Converts a [`TypeMode`] into the appropriate [`CallConvTag::CallMode`]
//     fn call_mode(&self, ty_mode: &TypeMode) -> Self::CallMode;

//     /// Maximum Size of values in the registers for the [`CallConvTag::CallMode`]
//     fn register_width(&self, mode: Self::CallMode) -> u64;
// }
