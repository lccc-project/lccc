use xlang::{
    abi::{collection::HashSet, string::StringView, span::Span},
    prelude::v1::HashMap,
    targets::properties::TargetProperties,
};

#[derive(Debug)]
pub struct Cfg {
    pub keys: HashSet<String>,
    pub pairs: HashMap<String, String>,
}

impl Cfg {
    pub fn from_properties(
        properties: &TargetProperties,
        features: Span<StringView>,
        vendor: StringView,
        target: StringView,
    ) -> Self {
        let mut keys = HashSet::new();
        let mut pairs = HashMap::new();
        pairs.insert(String::from("target"), target.to_string());
        pairs.insert(String::from("target_vendor"), vendor.to_string());

        Self { keys, pairs }
    }
}
