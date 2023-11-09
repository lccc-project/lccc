use crate::interning::Symbol;

pub trait RlibSerializer: xlang::abi::io::Write {
    fn write_u8(&mut self, x: u8) -> xlang::abi::io::Result<()>;
    fn write_u16(&mut self, x: u16) -> xlang::abi::io::Result<()>;
    fn write_u32(&mut self, x: u32) -> xlang::abi::io::Result<()>;
    fn write_u64(&mut self, x: u64) -> xlang::abi::io::Result<()>;
    fn write_u128(&mut self, x: u128) -> xlang::abi::io::Result<()>;

    fn write_byte_order(&mut self) -> xlang::abi::io::Result<()>;

    fn write_padding(&mut self, align: usize) -> xlang::abi::io::Result<()>;

    fn write_i8(&mut self, x: i8) -> xlang::abi::io::Result<()> {
        self.write_u8(x as u8)
    }
    fn write_i16(&mut self, x: i16) -> xlang::abi::io::Result<()> {
        self.write_u16(x as u16)
    }
    fn write_i32(&mut self, x: i32) -> xlang::abi::io::Result<()> {
        self.write_u32(x as u32)
    }
    fn write_i64(&mut self, x: i64) -> xlang::abi::io::Result<()> {
        self.write_u64(x as u64)
    }
    fn write_i128(&mut self, x: i128) -> xlang::abi::io::Result<()> {
        self.write_u128(x as u128)
    }

    fn write_sym(&mut self, x: Symbol) -> xlang::abi::io::Result<()>;
}

pub trait RlibSerializeable {
    fn write<S: RlibSerializer>(&self, serialize: S) -> xlang::abi::io::Result<()>;
}

pub struct RlibFileBuilder<W> {
    underlying: W,
    file_offset: usize,
    strtab: Vec<u8>,
}
