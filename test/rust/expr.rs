extern "C" {
    fn exit(code: i32);
}

fn main() {
    exit(4 / 2 - 2 * 3 + 4);
}
