use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::{self, Formatter},
};

use crate::{agc::Instruction, ast::Directive, loader::Program};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: &Program) -> anyhow::Result<Output> {
        let mut output = Output::new();
        let mut label_generator = LabelGenerator::new();

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

        for unit in &program.compilation_units {
            for directive in &unit.fragment().directives {
                match directive {
                    Directive::Include { .. } => {}
                    Directive::UserProgram { .. } => {}
                    Directive::State {
                        name,
                        parameters,
                        values,
                        body,
                    } => {
                        let label = label_generator.generate('S');
                        output
                            .code
                            .enqueue_comment(format!("STATE ::{}", name))
                            .enqueue_label(label)
                            .enqueue_line_break()
                            .append(Instruction::NOOP);
                    }
                    Directive::Subroutine { name, parameters, body } => {
                        let label = label_generator.generate('F');
                        output
                            .code
                            .enqueue_comment(format!("SUBROUTINE ::{}", name))
                            .enqueue_label(label)
                            .enqueue_line_break()
                            .append(Instruction::NOOP);
                    }
                }
            }
        }

        Ok(output)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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
        let code = self.code.pretty();
        let data = self.data.pretty();
        format!(include_str!("embed.agc.in"), code = code, data = data, entrypoint = "SYSMAIN")
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub fn pretty<'a>(&'a self) -> impl std::fmt::Display + 'a {
        PrettyArchive { archive: self }
    }

    pub fn append<'a>(&'a mut self, instruction: Instruction) -> InstructionDecorator<'a> {
        let position = self.instructions.len();
        self.instructions.push(instruction);
        InstructionDecorator { archive: self, position }
    }

    pub fn reserve_word<'a>(&'a mut self, label: Label) -> InstructionDecorator<'a> {
        self.enqueue_label(label).append(Instruction::ERASE)
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
    pub fn with_comment(self, comment: &str) -> Self {
        self.archive.comments.insert(self.position, comment.to_string());
        self
    }
}

struct PrettyArchive<'a> {
    archive: &'a Archive,
}

impl std::fmt::Display for PrettyArchive<'_> {
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
                writeln!(f, "{:.8}\t{}", label, instruction)?;
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
