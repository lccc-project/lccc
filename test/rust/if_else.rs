extern "C" {
    fn puts(str: *const u8) -> i32;
}

pub fn test_number(x: i32) {
    if x < 0 {
        puts(b"Number is negative\0" as *const u8);
    } else if x > 0 {
        puts(b"Number is positive\0" as *const u8);
    } else {
        puts(b"Number is zero\0" as *const u8);
    }
    // puts(if x & 1 == 0 {
    //     b"Number is even\0" as *const u8
    // } else {
    //     b"Number is odd\0" as *const u8
    // });
}

fn main() {
    // test_number(-256);
    // test_number(-1);
    // test_number(-0); // should still be 0
    test_number(0);
    test_number(1);
    test_number(256);
}
