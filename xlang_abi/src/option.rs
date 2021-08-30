#[repr(u8)]
pub enum Option<T> {
    Some(T),
    None,
}
