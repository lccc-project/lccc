

#[lang = "not"]
pub trait Not{
    type Output;
    fn not(self)->Self::Output;
}

