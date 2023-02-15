use xlang::{
    abi::{collection::HashSet, span::Span, string::StringView},
    prelude::v1::HashMap,
    targets::properties::TargetProperties,
};

#[derive(Debug)]
pub struct Cfg {
    pub keys: HashSet<String>,
    pub pairs: HashMap<String, String>,
}

impl Cfg {
    #[allow(dead_code)]
    pub fn from_properties(
        _properties: &TargetProperties,
        _features: Span<StringView>,
        vendor: StringView,
        target: StringView,
    ) -> Self {
        let keys = HashSet::new();
        let mut pairs = HashMap::new();
        pairs.insert(String::from("target"), target.to_string());
        pairs.insert(String::from("target_vendor"), vendor.to_string());

        Self { keys, pairs }
    }
}
