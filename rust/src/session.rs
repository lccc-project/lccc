use xlang::{
    abi::{collection::HashSet, span::Span, string::StringView},
    prelude::v1::HashMap,
    targets::properties::TargetProperties,
};

use crate::interning::Symbol;

fn single<S: Into<Symbol>>(x: S) -> HashSet<Symbol> {
    let mut set = HashSet::new();

    let _ = set.insert(x.into());

    set
}

fn list<I: IntoIterator>(i: I) -> HashSet<Symbol>
where
    I::Item: Into<Symbol>,
{
    i.into_iter().map(Into::into).collect()
}

#[derive(Debug)]
pub struct Cfg {
    keys: HashSet<Symbol>,
    pairs: HashMap<Symbol, HashSet<Symbol>>,
}

impl Cfg {
    #[allow(dead_code)]
    pub fn from_properties(properties: &TargetProperties, features: Span<StringView>) -> Self {
        let mut keys = HashSet::new();
        let mut pairs = HashMap::new();
        let _ = pairs.insert("target_arch".into(), list(properties.arch.arch_names));
        let _ = pairs.insert("target_os_family".into(), list(properties.os.os_family));
        let _ = pairs.insert("target_features".into(), list(features));

        if properties.os.is_unix_like {
            let _ = keys.insert("unix".into());
        } else if properties.os.is_windows_like {
            let _ = keys.insert("windows".into());
        }

        Self { keys, pairs }
    }
}
