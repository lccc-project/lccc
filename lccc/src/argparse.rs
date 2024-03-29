use xlang::prelude::v1::*;

#[allow(dead_code)]
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum TakesArg {
    Always,
    Optionally,
    Never,
}

#[derive(Clone, Debug, Hash)]
pub struct ArgSpec {
    name: &'static str,
    long: Vec<&'static str>,
    short: Vec<char>,
    takes_arg: TakesArg,
    once: bool,
}

impl ArgSpec {
    pub fn new(
        name: &'static str,
        long: Vec<&'static str>,
        short: Vec<char>,
        takes_arg: TakesArg,
        once: bool,
    ) -> Self {
        Self {
            name,
            long,
            short,
            takes_arg,
            once,
        }
    }
}

#[derive(Clone, Debug, Hash)]
pub struct Arg {
    pub name: &'static str,
    pub value: Option<String>,
}

#[allow(clippy::needless_borrow)] // Incorrect lint
pub fn parse_args(argspecs: &Vec<ArgSpec>) -> (Vec<Arg>, Vec<String>) {
    let mut result = Vec::new();
    let mut files = Vec::new();
    let mut args = std::env::args();
    let _program_name = args.next();
    while let std::option::Option::Some(arg) = args.next() {
        if arg.starts_with('-') && arg != "-" {
            // "-" is a special argument
            if let std::option::Option::Some(arg) = arg.strip_prefix("--") {
                let mut found = false;
                for spec in argspecs {
                    for long in &spec.long {
                        if arg.starts_with(*long) {
                            found = true;
                            if spec.takes_arg == TakesArg::Never {
                                result.push(Arg {
                                    name: spec.name,
                                    value: None,
                                });
                            } else if spec.takes_arg == TakesArg::Always {
                                let opt = args.next().unwrap();
                                result.push(Arg {
                                    name: spec.name,
                                    value: Some((&opt).into()),
                                });
                            } else {
                                todo!();
                            }
                            break;
                        }
                    }
                }
                if !found {
                    eprintln!("error: Unrecognized long option \"--{}\"", arg);
                    std::process::exit(1);
                }
            } else {
                let mut arg = arg.chars().skip(1); // Skip the dash
                let mut found = false;
                'outer: while let std::option::Option::Some(opt) = arg.next() {
                    for spec in argspecs {
                        if spec.short.contains(&opt) {
                            found = true;
                            if spec.takes_arg == TakesArg::Always {
                                let mut remainder: std::string::String = arg.by_ref().collect();
                                if remainder.is_empty() {
                                    args.next().map_or_else(|| {
                                        eprintln!("error: Got short option \"-{}\" without required parameter", opt);
                                        std::process::exit(1);
                                    }, |next| {
                                        remainder = next;
                                    });
                                }
                                result.push(Arg {
                                    name: spec.name,
                                    value: Some(String::from(&remainder)),
                                });
                                break 'outer;
                            }
                            result.push(Arg {
                                name: spec.name,
                                value: None,
                            });

                            break;
                        }
                    }
                    if !found {
                        eprintln!("error: Unrecognized short option \"-{}\"", opt);
                        std::process::exit(1);
                    }
                }
            }
        } else {
            files.push(String::from(&arg));
        }
    }
    (result, files)
}
