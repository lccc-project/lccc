use std::env::Args;
use std::io;

mod config;
mod dep;
mod programs;
mod tools;

fn main() {
    let mut args = std::env::args();

    let mut prg_name = args.next().unwrap();

    if prg_name == "cargo" {
        prg_name += &args.next().unwrap();
    }

    match real_main(args) {
        Ok(()) => {}
        Err(e) => {
            eprintln!("{}: {}", prg_name, e)
        }
    }
}

fn real_main(mut args: Args) -> io::Result<()> {
    let subcommand = args
        .next()
        .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidInput, "Expected a subcommand"))?;

    let subcommand_entry = tools::find_tool(&subcommand)?;

    subcommand_entry(args)
}
