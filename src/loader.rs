use std::{collections::HashMap, marker::PhantomData, path::PathBuf, rc::Rc};

use crate::ast::{Directive, ProgramFragment};

pub struct Program {
    pub compilation_units: Vec<CompilationUnit>,
}

pub struct CompilationUnit {
    fragment: Rc<ProgramFragment>,
    namespace_lookup: HashMap<String, Rc<ProgramFragment>>,
}

impl CompilationUnit {
    pub fn fragment(&self) -> &ProgramFragment {
        &self.fragment
    }
}

pub struct SourceLoader(PhantomData<()>);

impl SourceLoader {
    pub fn new() -> Self {
        Self(PhantomData)
    }

    pub fn parse_and_load<R: SourceReader>(&self, reader: R, entry_point: &PathBuf) -> anyhow::Result<Program> {
        let source = reader.read(&entry_point)?;
        let fragment = crate::parser::parse(&source)?;

        // TODO: Multiple file support!
        if fragment.directives.iter().any(|d| matches!(d, Directive::Include { .. })) {
            unimplemented!("Multiple file support is not implemented yet");
        }

        let main_compilation_unit = CompilationUnit {
            fragment: Rc::new(fragment),
            namespace_lookup: HashMap::new(),
        };

        Ok(Program {
            compilation_units: vec![main_compilation_unit],
        })
    }
}

pub trait SourceReader {
    fn read(&self, path: &PathBuf) -> anyhow::Result<String>;
}

pub struct FileSystemSourceReader {
    root: PathBuf,
}

impl FileSystemSourceReader {
    pub fn new(root: PathBuf) -> Self {
        Self { root }
    }

    pub fn new_with_cwd() -> anyhow::Result<Self> {
        Ok(Self::new(std::env::current_dir()?))
    }
}

impl SourceReader for FileSystemSourceReader {
    fn read(&self, path: &PathBuf) -> anyhow::Result<String> {
        let full_path = self.root.join(path);
        std::fs::read_to_string(full_path).map_err(|e| e.into())
    }
}
