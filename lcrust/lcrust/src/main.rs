use std::env;
use std::fs::File;
use std::io::prelude::*;

static mut PROGNAME: String = String::new();

fn log(text: String) {
    if text.contains('\n') {
        for sub in text.split('\n') {
            log(sub.to_string());
        }
    } else {
        unsafe {
            println!("{}: {}", PROGNAME, text);
        }
    }
}

fn fatal(text: String) {
    if text.contains('\n') {
        for sub in text.split('\n') {
            fatal(sub.to_string());
        }
    } else {
        unsafe {
            println!("{}: fatal error: {}", PROGNAME, text);
        }
    }
}

fn exit(code: i32) -> ! {
    std::process::exit(code);
}

fn main() {
    let mut args = env::args();

    unsafe { PROGNAME = args.next().unwrap().split('/').last().unwrap().to_string(); }

    for arg in args {
        let mut curfile = match File::open(&arg) {
            Ok(curfile) => curfile,
            Err(_e) => { fatal(format!("{}: no such file or directory", arg)); exit(1) }
        };
        let mut data = String::new();
        match curfile.read_to_string(&mut data) {
            Ok(_) => (),
            Err(_) => { fatal(format!("{}: Permission denied", arg)); exit(1) }
        }
        log(data);
    }
}
