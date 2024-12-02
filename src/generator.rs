use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::Formatter,
};

use crate::agc::{Address, Instruction};

pub struct Generator {
    output: Output,
}

impl Generator {
    pub fn new() -> Self {
        Self { output: Output::new() }
    }

    pub fn data_mut(&mut self) -> &mut Archive {
        &mut self.output.data
    }

    pub fn code_mut(&mut self) -> &mut Archive {
        &mut self.output.code
    }

    pub fn copy_slot_to_reg_a(&mut self, slot: Slot) {
        if slot.location == Location::Fixed {
            self.output.code.append(Instruction::CAF(slot.into()));
        } else {
            self.output.code.append(Instruction::CAE(slot.into()));
        }
    }

    pub fn copy_reg_a_to_slot(&mut self, slot: Slot) {
        if slot.location == Location::Fixed {
            panic!("Cannot write to a fixed location");
        } else {
            self.output.code.append(Instruction::XCH(slot.into()));
        }
    }

    pub fn copy_slot_to_reg_q(&mut self, slot: Slot) {
        if slot.location == Location::Fixed {
            self.copy_slot_to_reg_a(slot);
            self.output.code.append(Instruction::QXCH(Address::Relative {
                label: Label::from_static("A"),
                offset: 0,
            }));
        } else {
            self.output.code.append(Instruction::QXCH(slot.into()));
        }
    }

    pub fn copy_reg_q_to_slot(&mut self, slot: Slot) {
        if slot.location == Location::Fixed {
            panic!("Cannot write to a fixed location");
        } else {
            self.output.code.append(Instruction::QXCH(slot.into()));
        }
    }

    pub fn into_output(self) -> Output {
        self.output
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
        if let Some(existing_comment) = self.comments.get_mut(&self.instructions.len()) {
            existing_comment.push_str("\n");
            existing_comment.push_str(&comment.into());
        } else {
            self.comments.insert(self.instructions.len(), comment.into());
        }

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
        if let Some(existing_comment) = self.archive.comments.get_mut(&self.position) {
            existing_comment.push_str("\n");
            existing_comment.push_str(&comment.into());
        } else {
            self.archive.comments.insert(self.position, comment.into());
        }

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

pub struct ArchiveWriter<'a> {
    archive: &'a Archive,
}

impl std::fmt::Display for ArchiveWriter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let archive = self.archive;

        for (i, instruction) in archive.instructions.iter().enumerate() {
            if archive.line_breaks.contains(&i) {
                writeln!(f)?;
            }

            let (comment, label) = (archive.comments.get(&i), archive.labels.get(&i));

            if let Some(comment) = comment {
                let padding = if label.is_some() { "" } else { "\t\t" };
                for line in comment.lines() {
                    writeln!(f, "{}# {}", padding, line)?;
                }
            }

            if let Some(label) = label {
                write!(f, "{:.8}\t", label)?;
                match label {
                    Label::Static(s) if s.len() < 8 => write!(f, "\t")?,
                    _ => {}
                }

                if instruction.is_extend() {
                    writeln!(f, "EXTEND")?;
                }
                writeln!(f, "\t\t{}", instruction)?;
            } else if let Instruction::Literal(s) = instruction {
                writeln!(f, "{}", s)?;
            } else {
                if instruction.is_extend() {
                    writeln!(f, "\t\tEXTEND")?;
                }
                writeln!(f, "\t\t{}", instruction)?;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Slot {
    pub label: Label,
    pub offset: i32,
    pub location: Location,
}

impl Into<Address> for Slot {
    fn into(self) -> Address {
        Address::Relative {
            label: self.label,
            offset: self.offset,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Location {
    Erasable,
    Fixed,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Label {
    Static(Cow<'static, str>),
    Generated { prefix: char, identifier: usize },
}

impl Label {
    pub fn from_static(s: impl Into<Cow<'static, str>>) -> Self {
        Label::Static(s.into())
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Label::Static(identifier) => write!(f, "{:.8}", identifier),
            Label::Generated { prefix, identifier } => write!(f, "{}{:0>7X}", prefix, identifier),
        }
    }
}
