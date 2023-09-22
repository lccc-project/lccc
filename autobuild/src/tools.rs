use std::env::Args;
use std::io;

macro_rules! def_tools{
    {
        $(tool $tool:ident;)*
    } => {
        $(pub mod $tool;)*

        pub fn find_tool(x: &str) -> io::Result<fn(Args)->io::Result<()>>{
            match x{
                $(::core::stringify!($tool) => Ok($tool::main),)*
                x => Err(io::Error::new(io::ErrorKind::InvalidInput, format!("No such subcommand {}", x)))
            }
        }
    }
}

def_tools! {}
