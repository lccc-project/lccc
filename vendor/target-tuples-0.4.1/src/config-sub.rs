use target_tuples::*;

pub fn main() {
    let mut args = std::env::args();
    let bin = args.next().unwrap();
    if let Some(name) = args.next() {
        match &*name {
            "--help" => {
                println!("Usage: {} [OPTION]|<target>", bin);
                println!("Converts a target tuple into canonical form");
                println!("Options:");
                println!("\t--help: Prints this message, and exits");
                println!("\t--version: Prints version information, and exists");
            }
            "--version" => {
                println!("config.sub v{}", env!("CARGO_PKG_VERSION"));
                println!("Copyright (C) 2020 Connor Horman, this program is a free software, dual-licensed under the terms of the Apache v2 and the MIT license");
                println!("This program is provided AS-IS, without any warranty.");
            }
            x => {
                if let Ok(t) = x.parse::<Target>() {
                    println!("{}", t);
                } else {
                    eprintln!("Unsupported target {}", x);
                    std::process::exit(1);
                }
            }
        }
    } else {
        std::process::exit(1);
    }
}
