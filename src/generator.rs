
pub struct Generator;

use crate::compiler;

pub struct GeneratorError {
}

impl From<std::io::Error> for GeneratorError {
    fn from(_: std::io::Error) -> Self {
        GeneratorError {  }
    }
}

impl Generator {
    pub fn try_generate<T>(program: &compiler::ProgramCompilation<'_>, writer: &mut T) -> Result<(), GeneratorError> where T: std::io::Write {
        write!(writer, "Number of modules: {}", program.modules.len())?;
        Ok(())
    }
}
