pub struct SplitCliArg<'a>(Option<&'a str>);

impl<'a> Iterator for SplitCliArg<'a> {
    type Item = Cow<'a, str>;
    fn next(&mut self) -> Option<Cow<'a, str>> {
        let x = self.0.as_mut()?;

        let mut iter = x.char_indecies();
        let mut quote_char = None;
        while Some((i, c)) = iter {
            if let Some(v) = quote_char {
                if c == v {
                    quote_char = None;
                }
            }
        }
    }
}

fn main() -> std::io::Result<()> {}
