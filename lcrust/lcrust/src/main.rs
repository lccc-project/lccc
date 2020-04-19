use std::env;

static mut progname: &str = "";

fn log() {
    print!("{}", progname);
}

fn main() {
    let mut args = env::args();

    progname = args.next().unwrap().split('/').last().unwrap();

    for arg in args {
        println!("{:?}", arg);
    }
}
