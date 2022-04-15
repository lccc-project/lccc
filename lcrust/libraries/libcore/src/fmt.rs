pub struct Error(());

pub type Result = core::result::Result<(), Error>;

trait FMTTrait {
    fn fmt(&self, f: &mut Formatter) -> Result;
}
