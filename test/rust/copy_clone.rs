pub trait Clone {
    fn clone(self: &Self) -> Self;
}

#[lang = "copy"]
pub trait Copy: Clone {}

pub struct Foo {}

impl Clone for Foo {
    fn clone(self: &Self) -> Self {
        Foo {}
    }
}

impl Copy for Foo {}

fn main() {}
