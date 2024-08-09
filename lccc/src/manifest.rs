pub mod parse;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum MatcherFragment<'a> {
    String(&'a str),
    Glob(&'a str, Box<MatcherFragment<'a>>),
    GlobChar(&'a str, Box<MatcherFragment<'a>>),
}

impl<'a> MatcherFragment<'a> {
    pub fn match_length(&self) -> (usize, Option<usize>) {
        match self {
            MatcherFragment::String(val) => (val.len(), Some(val.len())),
            MatcherFragment::Glob(left, right) => {
                let (right_min, _) = right.match_length();

                (left.len() + right_min, None)
            }
            MatcherFragment::GlobChar(left, right) => {
                let (right_min, right_max) = right.match_length();

                (
                    right_min + 1 + left.len(),
                    right_max.map(|max| max + 1 + left.len()),
                )
            }
        }
    }
    pub fn matches(&self, val: &'a str) -> bool {
        match self {
            MatcherFragment::String(x) => *x == val,
            MatcherFragment::Glob(left, right) => {
                if let Some(x) = val.strip_prefix(*left) {
                    let (right_min, right_max) = right.match_length();

                    let x_len = x.len();

                    let possible_base = if let Some(right_max) = right_max {
                        let mut off = x_len.saturating_sub(right_max);
                        while !x.is_char_boundary(off) {
                            off += 1;
                        }
                        &x[off..]
                    } else {
                        x
                    };

                    let mut iter = possible_base.chars();

                    loop {
                        if iter.as_str().len() < right_min {
                            break false;
                        } else if right.matches(iter.as_str()) {
                            break true;
                        } else {
                            iter.next();
                        }
                    }
                } else {
                    false
                }
            }
            MatcherFragment::GlobChar(left, right) => {
                if let Some(x) = val.strip_prefix(*left) {
                    if x.is_empty() {
                        false
                    } else {
                        let mut iter = x.chars();
                        iter.next();
                        right.matches(iter.as_str())
                    }
                } else {
                    false
                }
            }
        }
    }

    pub fn parse(x: &'a str) -> Self {
        if let Some(val) = x.find(['*', '?']) {
            let (l, r) = x.split_at(val);
            let (c, r) = r.split_at(1); // We've matched an ASCII Char, so it's exactly 1 byte long

            let right = Box::new(Self::parse(r));

            match c {
                "*" => MatcherFragment::Glob(l, right),
                "?" => MatcherFragment::GlobChar(l, right),
                _ => unreachable!(),
            }
        } else {
            Self::String(x)
        }
    }
}

#[derive(Clone, Debug)]
pub struct SelectorManifest<'a> {
    pub lines: Vec<PluginSelector<'a>>,
}

impl<'a> SelectorManifest<'a> {
    pub fn parse(x: &'a str) -> Self {
        Self {
            lines: x
                .lines()
                .map(|x| {
                    if let Some((x, _comment)) = x.split_once(';') {
                        x
                    } else {
                        x
                    }
                })
                .map(str::trim)
                .filter(|x| !x.is_empty())
                .map(PluginSelector::parse)
                .collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub struct PluginSelector<'a> {
    pub matchers: Vec<MatcherFragment<'a>>,
    pub plugin: &'a str,
}

impl<'a> PluginSelector<'a> {
    pub fn parse(x: &'a str) -> Self {
        if let Some((matchers, plugins)) = x.split_once(':') {
            let matchers = matchers
                .split(',')
                .map(|x| x.trim())
                .map(MatcherFragment::parse)
                .collect();

            let plugin = plugins.trim();
            Self { matchers, plugin }
        } else {
            panic!("Invalid plugin")
        }
    }
}
