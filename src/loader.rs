use std::{collections::HashMap, marker::PhantomData, path::PathBuf};

use crate::ast::{Directive, ProgramFragment};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModuleIdentifier(String);

pub struct Program {
    pub compilation_units: HashMap<ModuleIdentifier, CompilationUnit>,
}

impl Program {
    pub fn new() -> Self {
        Self {
            compilation_units: HashMap::new(),
        }
    }

    pub fn new_with_units(units: impl IntoIterator<Item = CompilationUnit>) -> Self {
        Self {
            compilation_units: units.into_iter().map(|unit| (unit.identifier.clone(), unit)).collect(),
        }
    }

    pub fn new_with_source(source: SourceReference, fragment: ProgramFragment) -> Self {
        let identifier = source.to_string();
        let mut compilation_units = HashMap::new();
        compilation_units.insert(
            ModuleIdentifier(identifier.clone()),
            CompilationUnit {
                identifier: ModuleIdentifier(identifier),
                reference: source,
                aliases: HashMap::new(),
                fragment,
            },
        );

        Self { compilation_units }
    }
}

pub struct CompilationUnit {
    pub identifier: ModuleIdentifier,
    pub reference: SourceReference,
    pub fragment: ProgramFragment,
    pub aliases: HashMap<String, ModuleIdentifier>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SourceReference {
    Path(PathBuf),
    Labelled(String),
}

impl std::fmt::Display for SourceReference {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use SourceReference::*;
        match self {
            Path(path) => write!(f, "{}", path.display()),
            Labelled(label) => write!(f, "{}", label),
        }
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

        let identifier = String::from("main");

        let mut compilation_units = HashMap::new();
        compilation_units.insert(
            ModuleIdentifier(identifier.clone()),
            CompilationUnit {
                identifier: ModuleIdentifier(identifier),
                reference: SourceReference::Path(entry_point.clone()),
                aliases: HashMap::new(),
                fragment,
            },
        );

        Ok(Program { compilation_units })
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
