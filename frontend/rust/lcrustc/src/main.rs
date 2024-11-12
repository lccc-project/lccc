use lccc::{
    argparse::{parse_args, ArgSpec, TakesArg},
    LinkOutput, Mode, OptimizeLevel,
};

use xlang::prelude::v1::*;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CodegenOpt {
    OptimizeLevel(OptimizeLevel),
    LinkArg(String),
    LinkArgs(Vec<String>),
    TargetFeature(String, bool),
    TargetCpu(String),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum UnstableOpt {
    Intree,
}

fn main() {
    let mut target = lccc::get_default_target();
    let mut output = None::<String>;

    let mut mode = Mode::Link;
    let mut link_output = LinkOutput::Executable;

    let mut opt = OptimizeLevel::Integer(0);

    let argspecs = xlang::vec![
        ArgSpec::new(
            "output",
            Vec::new(),
            xlang::vec!['o'],
            TakesArg::Always,
            true
        ),
        ArgSpec::new(
            "optimize",
            Vec::new(),
            xlang::vec!['O'],
            TakesArg::Never,
            true
        ),
        ArgSpec::new(
            "codegen",
            Vec::new(),
            xlang::vec!['C'],
            TakesArg::Always,
            true
        ),
        ArgSpec::new(
            "unstable",
            Vec::new(),
            xlang::vec!['Z'],
            TakesArg::Always,
            true
        ),
        ArgSpec::new(
            "crate-name",
            xlang::vec!["crate-name"],
            Vec::new(),
            TakesArg::Always,
            true
        )
    ];

    let (args, files) = parse_args(&argspecs);

    for arg in args {
        match arg.name {
            "output" => output = Some(arg.value.unwrap()),
            "optimize" => opt = OptimizeLevel::Integer(2),
            opt => todo!("unsupported option: {}", opt),
        }
    }
}
