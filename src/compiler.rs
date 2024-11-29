use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::{self, Formatter},
};

use crate::{
    agc::Instruction,
    ast::{Block, Directive, ValueDeclaration, ValueDefinition},
    loader::{CompilationUnit, Program},
    types::{Numeric, Real},
};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: &Program) -> anyhow::Result<Output> {
        let mut output = Output::new();
        let mut label_generator = LabelGenerator::new();
        let mut constant_pool = ConstantPool::new();

        output.data.enqueue_comment("DSKY REGISTERS AND FLAGS");
        output.data.reserve_word(Label::new("DSKYREG1"));
        output.data.reserve_word(Label::new("DSKYSGN1"));
        output.data.reserve_word(Label::new("DSKYREG2"));
        output.data.reserve_word(Label::new("DSKYSGN2"));
        output.data.reserve_word(Label::new("DSKYREG3"));
        output.data.reserve_word(Label::new("DSKYSGN3"));
        output.data.reserve_word(Label::new("DSKYPROG"));
        output.data.reserve_word(Label::new("DSKYNOUN"));
        output.data.reserve_word(Label::new("DSKYVERB"));
        output.data.reserve_word(Label::new("DSKYMASK"));

        // TODO: Delete this
        _ = constant_pool.get_or_insert(Numeric::Real(Real::from(-123.456)));

        for unit in &program.compilation_units {
            for directive in &unit.fragment().directives {
                match directive {
                    Directive::Include { .. } => {}
                    Directive::UserProgram { .. } => {}
                    Directive::State {
                        name,
                        values,
                        parameters,
                        body,
                    } => {
                        let label: Label = label_generator.generate('S');
                        output
                            .code
                            .append(Instruction::NOOP)
                            .with_comment(format!("STATE :: {} ({})", name, unit.path().display()))
                            .with_label(label)
                            .with_preceding_line_break();

                        let workspace = Workspace::create(label_generator.generate('W'), parameters, values, body);
                        workspace.write(&mut output, unit, name);
                    }
                    Directive::Subroutine { name, parameters, body } => {
                        let label = label_generator.generate('F');
                        output
                            .code
                            .append(Instruction::NOOP)
                            .with_comment(format!("SUBROUTINE :: {} ({})", name, unit.path().display()))
                            .with_label(label)
                            .with_preceding_line_break();

                        let workspace = Workspace::create(label_generator.generate('W'), parameters, &[], body);
                        workspace.write(&mut output, unit, name);
                    }
                }
            }
        }

        if !constant_pool.constants.is_empty() {
            output.code.enqueue_line_break().enqueue_comment("CONSTANT POOL");
            for (constant, label) in constant_pool.constants.into_iter() {
                output.code.append(Instruction::DEC(constant)).with_label(label);
            }
        }

        Ok(output)
    }
}

struct Workspace {
    label: Label,
    values: HashMap<String, usize>,
    parameters: HashMap<String, usize>,
    temporaries: HashMap<String, usize>,
}

impl Workspace {
    fn create(label: Label, parameters: &[ValueDeclaration], values: &[ValueDefinition], _body: &Block) -> Self {
        let mut workspace = Self {
            label,
            values: HashMap::new(),
            parameters: HashMap::new(),
            temporaries: HashMap::new(),
        };

        for parameter in parameters.iter() {
            workspace
                .parameters
                .insert(parameter.identifier.clone(), workspace.slot_count());
        }

        for value in values.iter() {
            workspace
                .values
                .insert(value.declaration.identifier.clone(), workspace.slot_count());
        }

        workspace
    }

    fn slot_count(&self) -> usize {
        self.values.len() + self.parameters.len() + self.temporaries.len()
    }

    fn write(&self, output: &mut Output, unit: &CompilationUnit, block_name: &String) {
        if self.slot_count() > 0 {
            output
                .data
                .enqueue_line_break()
                .enqueue_comment(format!("WORKSPACE :: {} ({})", block_name, unit.path().display()))
                .enqueue_label(self.label.clone());

            for _ in 0..self.slot_count() {
                output.data.append(Instruction::ERASE);
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Output {
    pub data: Archive,
    pub code: Archive,
}

impl Output {
    pub fn new() -> Self {
        Self {
            data: Archive::new(),
            code: Archive::new(),
        }
    }

    pub fn to_yul_assembly(&self) -> String {
        let code = self.code.writer();
        let data = self.data.writer();
        format!(include_str!("embed.agc.in"), code = code, data = data, entrypoint = "SYSMAIN")
    }
}

#[derive(Debug, Clone)]
pub struct Archive {
    pub instructions: Vec<Instruction>,
    pub labels: HashMap<usize, Label>,
    pub comments: HashMap<usize, String>,
    pub line_breaks: HashSet<usize>,
}

impl Archive {
    pub fn new() -> Self {
        Self {
            instructions: Vec::new(),
            labels: HashMap::new(),
            comments: HashMap::new(),
            line_breaks: HashSet::new(),
        }
    }

    pub fn writer<'a>(&'a self) -> impl std::fmt::Display + 'a {
        ArchiveWriter { archive: self }
    }

    pub fn append<'a>(&'a mut self, instruction: Instruction) -> InstructionDecorator<'a> {
        let position = self.instructions.len();
        self.instructions.push(instruction);
        InstructionDecorator { archive: self, position }
    }

    pub fn reserve_word<'a>(&'a mut self, label: Label) -> InstructionDecorator<'a> {
        self.append(Instruction::ERASE).with_label(label)
    }

    pub fn enqueue_line_break<'a>(&'a mut self) -> &'a mut Self {
        self.line_breaks.insert(self.instructions.len());
        self
    }

    pub fn enqueue_comment<'a>(&'a mut self, comment: impl Into<String>) -> &'a mut Self {
        self.comments.insert(self.instructions.len(), comment.into());
        self
    }

    pub fn enqueue_label<'a>(&'a mut self, label: Label) -> &'a mut Self {
        self.labels.insert(self.instructions.len(), label);
        self
    }
}

pub struct InstructionDecorator<'a> {
    archive: &'a mut Archive,
    position: usize,
}

impl InstructionDecorator<'_> {
    pub fn with_comment(self, comment: impl Into<String>) -> Self {
        self.archive.comments.insert(self.position, comment.into());
        self
    }

    pub fn with_label(self, label: Label) -> Self {
        self.archive.labels.insert(self.position, label);
        self
    }

    pub fn with_preceding_line_break(self) -> Self {
        self.archive.line_breaks.insert(self.position);
        self
    }
}

struct ArchiveWriter<'a> {
    archive: &'a Archive,
}

impl std::fmt::Display for ArchiveWriter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let archive = self.archive;

        for (i, instruction) in archive.instructions.iter().enumerate() {
            if archive.line_breaks.contains(&i) {
                writeln!(f)?;
            }

            let (comment, label) = (archive.comments.get(&i), archive.labels.get(&i));

            if let Some(comment) = comment {
                if label.is_some() {
                    writeln!(f, "# {}", comment)?;
                } else {
                    writeln!(f, "\t\t# {}", comment)?;
                }
            }

            if let Some(label) = label {
                write!(f, "{:.8}\t", label)?;
                if label.as_str().len() <= 4 {
                    write!(f, "\t")?;
                }

                writeln!(f, "{}", instruction)?;
            } else {
                writeln!(f, "\t\t{}", instruction)?;
            }
        }

        Ok(())
    }
}

struct LabelGenerator {
    prefixes: HashMap<char, usize>,
}

impl LabelGenerator {
    pub fn new() -> Self {
        Self {
            prefixes: HashMap::new(),
        }
    }

    pub fn generate(&mut self, prefix: char) -> Label {
        let prefix = prefix.to_ascii_uppercase();
        let entry = self.prefixes.entry(prefix);
        let index = entry.or_insert(0);
        let label = Label::new(format!("{}{:0>7}", prefix, index));

        *index += 1;
        label
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Label(Cow<'static, str>);

impl Label {
    pub fn new(s: impl Into<Cow<'static, str>>) -> Self {
        Self(s.into())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

struct ConstantPool {
    constants: HashMap<Numeric, Label>,
}

impl ConstantPool {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }

    pub fn get_or_insert(&mut self, value: Numeric) -> Label {
        if let Some(label) = self.constants.get(&value) {
            return label.clone();
        }

        let label = Label::new(format!("C{:0>7}", self.constants.len()));
        self.constants.insert(value, label.clone());
        label
    }
}

#[cfg(test)]
mod test {
    use std::borrow::Cow;

    use crate::compiler::Label;

    #[test]
    fn test_static_label() {
        let label = Label::new("TEST");
        assert!(matches!(label.0, Cow::Borrowed(..)));
    }

    #[test]
    fn test_label_generator() {
        let mut generator = super::LabelGenerator::new();
        assert_eq!(generator.generate('t'), Label::new("T0000000"));
        assert_eq!(generator.generate('T'), Label::new("T0000001"));
        assert_eq!(generator.generate('B'), Label::new("B0000000"));
    }
}
