use std::env;
use std::fs::File;
use std::io::prelude::*;

extern crate xlang;

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
    std::process::exit(code)
}

enum Token {
    Keyword(String),
    Identifier(String),
    CharLiteral(char),
    StringLiteral(String),
    ByteLiteral(u8),
    ByteStringLiteral(Box<[u8]>)
}

fn lex(file: File) {
    let tokens = Vec::<Token>::new();
}

fn main() {
    let mut args = env::args();

    unsafe { PROGNAME = args.next().unwrap().split('/').last().unwrap().to_string(); }

    for arg in args {
        let mut curfile = match File::open(&arg) {
            Ok(curfile) => curfile,
            Err(_e) => { fatal(format!("{}: no such file or directory", arg)); exit(1) }
        };
        let tokens = lex(curfile);
    }
}
