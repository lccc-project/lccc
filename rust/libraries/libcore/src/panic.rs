pub struct PanicInfo<'a> {
    payload: &'a (dyn Any + Send),
    loc: &'a Location<'a>,
}

impl<'a> !Send for PanicInfo<'a> {}
impl<'a> !Sync for PanicInfo<'a> {}

impl<'a> PanicInfo<'a> {
    pub fn payload(&self) -> &(dyn Any + Send) {
        self.payload
    }
    #[unstable(feature = "panic_info_message", issue = "66754")]
    pub fn message(&self) -> Option<&Arguments> {
        self.payload.upcast()
    }
    pub fn location(&self) -> Option<&Location<'_>> {
        Some(self.loc)
    }
}

impl PanicInfo<'static> {
    #[unstable(feature = "lccc_construct_panic")]
    #[track_caller]
    pub fn construct_panic(payload: &'static (dyn Any + Send)) -> Self {
        Self {
            payload,
            location: Location::caller(),
        }
    }
}

pub struct Location<'a> {
    file: &'a str,
    line: u32,
    col: u32,
}

impl<'a> Location<'a> {
    #[track_caller]
    #[inline]
    pub fn caller() -> &'static Location<'static> {
        let (file, line, col) = ::__lccc::builtins::Rust::__builtin_caller_location();
        Self { file, line, col }
    }

    pub fn file(&self) -> &str {
        self.file
    }

    pub fn line(&self) -> u32 {
        self.line
    }

    pub fn col(&self) -> u32 {
        self.col
    }
}
