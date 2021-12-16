mod argparse;

use crate::argparse::{parse_args, ArgSpec, TakesArg};
use xlang::prelude::v1::*;

fn main() {
    let s = String::from("Hello World");
    println!("{}", s);

    let argspecs = xlang::vec![
        ArgSpec::new(
            "output",
            Vec::new(),
            xlang::vec!['o'],
            TakesArg::Always,
            true,
        ),
        ArgSpec::new(
            "intree",
            xlang::vec!["intree", "in-tree"],
            Vec::new(),
            TakesArg::Never,
            true
        )
    ];

    println!("{:?}", parse_args(argspecs))
}
