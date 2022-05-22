use xlang::ir::Type;

/// Basic Queries about Machine Features
pub trait MachineFeatures {
    /// Checks if the machine supports the given size of integer natively
    fn supports_int(&self, bitsize: u16) -> bool;
    /// Checks if the machine supports the given size of floating-point number natively
    fn supports_float(&self, bitsize: u16) -> bool;
    /// Checks if the machine supports the given size of vector natively
    fn supports_vector(&self, vectorbytes: u64) -> bool;
}

/// An abstract machine instruction, converted by `xlang_backend` from XIR.
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MCInsn {
    /// Move a value from one location to another
    Mov {
        /// The destination location
        dest: Location,
        /// The source location
        src: Location,
    },
    /// Moves an immediate value into a location
    MovImm {
        /// The destination location
        dest: Location,
        /// The Source Value
        src: u128,
    },
    /// Stores from src into the pointer in `dest_ptr`
    StoreIndirect {
        /// The destination pointer
        dest_ptr: Location,
        /// The Source value
        src: Location,
    },
}

/// A location (register) allocated by the backend
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct Location {
    id: u32,
    has_addr: bool,
    ty: Type,
}
