pub trait SplitOnceOwned: Sized {
    fn split_once_take(&mut self, pat: &str) -> Option<(Self, Self)>;
    fn split_once_owned(mut self, pat: &str) -> Result<(Self, Self), Self> {
        self.split_once_take(pat).ok_or(self)
    }
}

impl SplitOnceOwned for String {
    fn split_once_take(&mut self, pat: &str) -> Option<(Self, Self)> {
        if let Some(pos) = self.find(pat) {
            let mut new_str = Vec::new();

            let off = pos + pat.len();

            new_str.extend_from_slice(&self.as_bytes()[off..]);

            self.truncate(pos);

            Some((core::mem::take(self), unsafe {
                String::from_utf8_unchecked(new_str)
            }))
        } else {
            None
        }
    }
}
