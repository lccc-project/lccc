extern "C" {
    fn puts(str: *const u8) -> i32;
}

pub fn test_number(x: i32) {
    if x < 0 {
        puts("Number is negative");
    } else if x > 0 {
        puts("Number is positive");
    } else {
        puts("Number is zero");
    }
    puts(if x % 2 == 0 {
        "Number is even"
    } else {
        "Number is odd"
    });
}

fn main() {
    test_number(-256);
    test_number(-1);
    test_number(-0); // should still be 0
    test_number(0);
    test_number(1);
    test_number(256);
}
