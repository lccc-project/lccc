use std::env::Args;
use std::io;

macro_rules! def_tools{
    {
        $(tool $tool:ident;)*
    } => {
        $(pub mod $tool;)*

        pub fn find_tool(x: &str) -> io::Result<fn(&str, Args)->io::Result<()>>{
            match x{
                $(::core::stringify!($tool) => Ok($tool::main),)*
                x => Err(io::Error::new(io::ErrorKind::InvalidInput, format!("No such subcommand {}", x)))
            }
        }

        pub fn print_help(prg_name: &str, tool_name: &str, help_cb: fn()){
            println!("Usage: {} <subcommand>",prg_name);
            println!("Available Subcommands:");
            $(println!("\t{}", ::core::stringify!($tool));)*

            println!("{} {} Usage:", prg_name, tool_name);
            help_cb()
        }

        pub fn help_subcommands<T>() -> io::Result<T>{
            Err(io::Error::new(io::ErrorKind::InvalidInput, {
                use core::fmt::Write;
                let mut st = String::new();

                let _ = writeln!(st, "Subcommands:");
                $(let _ = writeln!(st, "\t{}", ::core::stringify!($tool));)*

                st
            }))
        }
    }
}

def_tools! {
    tool config;
}

pub fn print_version() {
    println!("cargo-autobuild v{}", env!("CARGO_PKG_VERSION"));
    println!("Copyright (C) 2024 LCCC Maintainers");
    println!("This Package is released under the terms of the 2BSD License + Patent Grant");
    println!("See LICENSE for details");
}

pub fn require_arg<I: Iterator<Item = String>>(flag: &str, args: &mut I) -> io::Result<String> {
    args.next().ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::InvalidInput,
            format!("{} requires an argument", flag),
        )
    })
}
