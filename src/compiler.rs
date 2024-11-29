use std::{
    borrow::Cow,
    collections::{HashMap, HashSet},
    fmt::{self, Formatter},
};

use crate::{
    agc::{Address, Instruction},
    ast::{Block, Directive, Expression, Statement, ValueDeclaration, ValueIdentifier},
    loader::{CompilationUnit, Program},
    types::{Numeric, Real},
};

pub struct Compiler {}

impl Compiler {
    pub fn new() -> Self {
        Self {}
    }

    pub fn compile(&self, program: &Program) -> anyhow::Result<Output> {
        State::new().compile(program)
    }
}

struct State {
    output: Output,
    label_generator: LabelGenerator,
    constant_pool: ConstantPool,
}

impl State {
    fn new() -> Self {
        Self {
            output: Output::new(),
            label_generator: LabelGenerator::new(),
            constant_pool: ConstantPool::new(),
        }
    }

    pub fn compile(mut self, program: &Program) -> anyhow::Result<Output> {
        self.output.data.enqueue_comment("DSKY REGISTERS AND FLAGS");
        self.output.data.reserve_word(Label::from_static("DSKYREG1"));
        self.output.data.reserve_word(Label::from_static("DSKYSGN1"));
        self.output.data.reserve_word(Label::from_static("DSKYREG2"));
        self.output.data.reserve_word(Label::from_static("DSKYSGN2"));
        self.output.data.reserve_word(Label::from_static("DSKYREG3"));
        self.output.data.reserve_word(Label::from_static("DSKYSGN3"));
        self.output.data.reserve_word(Label::from_static("DSKYPROG"));
        self.output.data.reserve_word(Label::from_static("DSKYNOUN"));
        self.output.data.reserve_word(Label::from_static("DSKYVERB"));
        self.output.data.reserve_word(Label::from_static("DSKYMASK"));

        // TODO: Delete this
        _ = self
            .constant_pool
            .get_or_insert(&mut &mut self.label_generator, Numeric::Real(Real::from(-123.456)));

        for unit in &program.compilation_units {
            for directive in &unit.fragment().directives {
                match directive {
                    Directive::Include { .. } => {}
                    Directive::UserProgram { .. } => {}
                    Directive::State {
                        name, parameters, body, ..
                    } => {
                        let label: Label = self.label_generator.generate('S');
                        let mut workspace = Workspace::create(self.label_generator.generate('W'), parameters, body);

                        self.output
                            .code
                            .enqueue_line_break()
                            .enqueue_label(label.clone())
                            .enqueue_comment(format!("STATE :: {} ({})", name, unit.path().display()));

                        self.compile_function(body, &mut workspace)?;

                        workspace.write(&mut self.output, unit, name);
                    }
                    Directive::Subroutine { name, parameters, body } => {
                        let label = self.label_generator.generate('F');
                        let mut workspace = Workspace::create(self.label_generator.generate('W'), parameters, body);

                        self.output
                            .code
                            .enqueue_line_break()
                            .enqueue_label(label.clone())
                            .enqueue_comment(format!("SUBROUTINE :: {} ({})", name, unit.path().display()));

                        self.compile_function(body, &mut workspace)?;

                        workspace.write(&mut self.output, unit, name);
                    }
                }
            }
        }

        if !self.constant_pool.constants.is_empty() {
            self.output.code.enqueue_line_break().enqueue_comment("CONSTANT POOL");
            for (constant, label) in self.constant_pool.constants.into_iter() {
                self.output.code.append(Instruction::DEC(constant)).with_label(label);
            }
        }

        Ok(self.output)
    }

    fn compile_function(&mut self, block: &Block, workspace: &mut Workspace) -> anyhow::Result<()> {
        // Store the Q register in the return slot so we can ret to it
        self.copy_reg_q_to_slot(workspace.ret_slot());

        for statement in block.statements.iter() {
            match statement {
                Statement::Definition(definition) => {
                    let lhs = workspace.create_named_slot(&definition.declaration.identifier);
                    let rhs = self.compile_expression(&definition.expression, workspace);
                    if let Some(rhs) = rhs {
                        self.copy_slot_to_reg_a(rhs);
                        self.copy_reg_a_to_slot(lhs);
                    } else {
                        anyhow::bail!("RHS expression does not produce a value");
                    }
                }
                Statement::Expression(expression) => {
                    self.compile_expression(expression, workspace);
                }
            }
        }

        // Put the return value back in the Q register and return
        self.copy_slot_to_reg_q(workspace.ret_slot());
        self.output.code.append(Instruction::RETURN);

        Ok(())
    }

    fn compile_expression(&mut self, expression: &Expression, workspace: &mut Workspace) -> Option<Slot> {
        match expression {
            Expression::NumberLiteral(value) => {
                let label = self.constant_pool.get_or_insert(&mut self.label_generator, value.clone());
                Some(Slot {
                    label,
                    offset: 0,
                    location: Location::Fixed,
                })
            }
            Expression::VariableReference(identifier) => match identifier {
                ValueIdentifier::Implicit(name) => workspace.resolve_named_slot(name),
                ValueIdentifier::Namespaced(..) => unimplemented!("Support for namespaced identifiers"),
            },
            _ => None,
        }
    }

    fn copy_slot_to_reg_a(&mut self, slot: Slot) {
        if slot.location == Location::Fixed {
            self.output.code.append(Instruction::CAF(slot.into()));
        } else {
            self.output.code.append(Instruction::CAE(slot.into()));
        }
    }

    fn copy_reg_a_to_slot(&mut self, slot: Slot) {
        if slot.location == Location::Fixed {
            panic!("Cannot write to a fixed location");
        } else {
            self.output.code.append(Instruction::XCH(slot.into()));
        }
    }

    fn copy_slot_to_reg_q(&mut self, slot: Slot) {
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

    fn copy_reg_q_to_slot(&mut self, slot: Slot) {
        if slot.location == Location::Fixed {
            panic!("Cannot write to a fixed location");
        } else {
            self.output.code.append(Instruction::QXCH(slot.into()));
        }
    }
}

struct Workspace {
    label: Label,
    parameters: HashMap<String, usize>,
    locals: HashMap<String, usize>,
}

impl Workspace {
    fn create(label: Label, parameters: &[ValueDeclaration], _body: &Block) -> Self {
        let mut workspace = Self {
            label,
            parameters: HashMap::new(),
            locals: HashMap::new(),
        };

        for parameter in parameters.iter() {
            workspace
                .parameters
                .insert(parameter.identifier.clone(), workspace.slot_count());
        }

        workspace
    }

    fn ret_slot(&self) -> Slot {
        Slot {
            label: self.label.clone(),
            offset: 0,
            location: Location::Erasable,
        }
    }

    fn slot_count(&self) -> usize {
        // Slot 0 is reserved for the return address
        self.parameters.len() + self.locals.len() + 1
    }

    fn create_named_slot(&mut self, name: &str) -> Slot {
        let slot = self.slot_count();
        self.locals.insert(name.to_string(), slot);
        Slot {
            label: self.label.clone(),
            offset: slot as i32,
            location: Location::Erasable,
        }
    }

    fn resolve_named_slot(&self, name: &str) -> Option<Slot> {
        if let Some(slot) = self.parameters.get(name) {
            return Some(Slot {
                label: self.label.clone(),
                offset: *slot as i32,
                location: Location::Erasable,
            });
        }

        if let Some(slot) = self.locals.get(name) {
            return Some(Slot {
                label: self.label.clone(),
                offset: *slot as i32,
                location: Location::Erasable,
            });
        }

        None
    }

    fn write(&self, output: &mut Output, unit: &CompilationUnit, block_name: &String) {
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
                match label {
                    Label::Static(s) if s.len() <= 4 => write!(f, "\t")?,
                    _ => {}
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
    next_identifier: usize,
}

impl LabelGenerator {
    pub fn new() -> Self {
        Self { next_identifier: 0 }
    }

    pub fn generate(&mut self, prefix: char) -> Label {
        let prefix = prefix.to_ascii_uppercase();
        let label = Label::Generated {
            prefix,
            identifier: self.next_identifier,
        };

        self.next_identifier += 1;
        label
    }
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

impl fmt::Display for Label {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Label::Static(identifier) => write!(f, "{:.8}", identifier),
            Label::Generated { prefix, identifier } => write!(f, "{}{:0>7X}", prefix, identifier),
        }
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

    pub fn get_or_insert(&mut self, label_generator: &mut LabelGenerator, value: Numeric) -> Label {
        if let Some(label) = self.constants.get(&value) {
            return label.clone();
        }

        let label = label_generator.generate('C');
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
        let label = Label::from_static("TEST");
        assert!(matches!(label, Label::Static(Cow::Borrowed(..))));
        assert_eq!(label.to_string(), String::from("TEST"));
    }

    #[test]
    fn test_label_generator() {
        let mut generator = super::LabelGenerator::new();
        assert_eq!(
            generator.generate('T'),
            Label::Generated {
                prefix: 'T',
                identifier: 0
            }
        );

        assert_eq!(
            generator.generate('t'),
            Label::Generated {
                prefix: 'T',
                identifier: 1
            }
        );

        assert_eq!(generator.generate('t').to_string(), String::from("T0000002"));
    }
}
