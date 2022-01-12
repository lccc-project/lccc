use std::fs::File;

use std::io::{BufRead, BufReader};

use target_tuples::Target;

#[test]
fn test_many_config_subs() -> std::io::Result<()> {
    let f = BufReader::new(File::open(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/config-sub.data"
    ))?);
    for s in f.lines() {
        let s = s?;
        if s.trim().is_empty() {
            continue;
        }
        let mut s = s.split('|');
        let (k, v) = (s.next().unwrap(), s.next().unwrap());
        let targ = Target::parse(k);
        assert_eq!(targ.to_string(), v);
    }

    Ok(())
}

#[test]
#[ignore]
fn test_rustc_names() -> std::io::Result<()> {
    let f = BufReader::new(File::open(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/rustc-targets.data"
    ))?);
    for s in f.lines() {
        let s = s?;
        if s.trim().is_empty() {
            continue;
        }
        Target::parse(&s);
    }
    Ok(())
}

#[test]
fn test_idempotency() -> std::io::Result<()> {
    let f = BufReader::new(File::open(concat!(
        env!("CARGO_MANIFEST_DIR"),
        "/tests/config-sub.data"
    ))?);
    for s in f.lines() {
        let s = s?;
        let s = s.trim();
        if s.is_empty() || s.starts_with('#') {
            continue;
        }
        let mut s = s.split('|');
        let (_, v) = (s.next().unwrap(), s.next().unwrap());
        let targ = Target::parse(v);
        assert_eq!(targ.to_string(), v);
    }

    Ok(())
}
