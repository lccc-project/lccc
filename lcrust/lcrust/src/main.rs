use std::env;

static mut PROGNAME: &str = "";

fn log(text: String) {
    unsafe { print!("{}", PROGNAME); }
    println!("{}", text);
}

fn main() {
    let mut args = env::args();

    unsafe { PROGNAME = args.next().unwrap().split('/').last().unwrap(); }

    for arg in args {
        log(format!("{:?}", arg));
    }
}
