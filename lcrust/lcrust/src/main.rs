use std::env;

static mut PROGNAME: String = String::new();

fn log(text: String) {
    unsafe {
        println!("{}: {}", PROGNAME, text);
    }
}

fn main() {
    let mut args = env::args();

    unsafe { PROGNAME = args.next().unwrap().split('/').last().unwrap().to_string(); }

    for arg in args {
        log(format!("{:?}", arg));
    }
}
