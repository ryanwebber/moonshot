use crate::agc;
use crate::compiler;
use crate::ir;
use crate::optimizer;
use crate::utils;

use std::collections::HashMap;
use std::io::Write;

#[derive(Clone)]
enum AssemblyEntry {
    Comment(String),
    Instruction(agc::Instruction, Option<Operand>),
    Label(Label),
    Raw(String),
}

struct AssemblyGenerator<'a> {
    code: &'a mut AssemblyWriter,
    data: &'a mut AssemblyWriter,
}

pub struct AssemblyPackage {
    erasable_source: Vec<AssemblyEntry>,
    fixed_source: Vec<AssemblyEntry>,
}

struct AssemblyWriter {
    buf: Vec<AssemblyEntry>,
}

pub struct Generator;

pub enum GeneratorError {
    IoError(std::io::Error),
    LabelOverflow,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Id(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Label {
    id: Id,
    kind: LabelType,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum LabelType {
    Const,
    Procedure,
    Stack,
    Workspace,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Operand {
    Slot(Slot),
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Slot {
    label: Label,
    offset: usize,
}

struct WorkspaceArtifact {
    stack_size: usize,
}

struct VirtualStack {
    id: Id,
    offset: usize,
    maximum_size: usize,
}

pub type WriterError = std::io::Error;

impl<'a> AssemblyGenerator<'a> {
    fn new(code_writer: &'a mut AssemblyWriter, data_writer: &'a mut AssemblyWriter) -> Self {
        Self {
            code: code_writer,
            data: data_writer,
        }
    }

    fn try_push_const_expr(&mut self, expr: &ir::ConstExpr) -> Result<(), GeneratorError> {
        let const_slot: Slot = Label::with(expr.id, LabelType::Const).into();
        self.code.push_instruction(agc::Instruction::CAF, Some(Operand::Slot(const_slot)));
        Ok(())
    }

    fn try_push(&mut self, rval: &ir::RVal, stack: &mut VirtualStack) -> Result<(), GeneratorError> {
        let dest_slot = stack.push();
        match rval {
            ir::RVal::Add(expr) => {
                self.try_push(&*expr.lhs, stack)?;
                self.try_push(&*expr.rhs, stack)?;
                self.try_pop(stack)?;
                self.code.push_instruction(agc::Instruction::ADS, Some(Operand::Slot(dest_slot)));
                stack.offset -= 1;

                Ok(())
            }
            ir::RVal::Const(expr) => {
                self.try_push_const_expr(expr)?;
                self.code.push_instruction(agc::Instruction::TS, Some(Operand::Slot(dest_slot)));
                Ok(())
            }
            ir::RVal::Reg(expr) => {
                let source_slot = Slot::from(expr.reg);
                self.code.push_instruction(agc::Instruction::CAE, Some(Operand::Slot(source_slot)));
                self.code.push_instruction(agc::Instruction::TS, Some(Operand::Slot(dest_slot)));
                Ok(())
            }
        }
    }

    fn try_pop(&mut self, stack: &mut VirtualStack) -> Result<Slot, GeneratorError> {
        let slot = stack.pop();
        self.code.push_instruction(agc::Instruction::CAE, Some(Operand::Slot(slot.clone())));
        Ok(slot)
    }

    fn try_generate_instruction(&mut self, instruction: &ir::Instruction, stack: &mut VirtualStack) -> Result<(), GeneratorError> {
        match instruction {
            ir::Instruction::Set(lhs, rhs) => {
                let register = match lhs {
                    ir::LVal::Reg(expr) => expr.reg,
                };

                let dest_slot = Slot::from(register);
                self.try_push(rhs, stack)?;
                self.try_pop(stack)?;
                self.code.push_instruction(agc::Instruction::TS, Some(Operand::Slot(dest_slot)));

                Ok(())
            }
        }
    }

    fn try_generate_procedure(&mut self, id: ir::Id, procedure: &compiler::ProcedureDefinition) -> Result<WorkspaceArtifact, GeneratorError> {
        let mut stack = VirtualStack::new(Id::from(id));

        self.code.push_comment(format!("{}", procedure.prototype.signature));

        // Write the function label
        self.code.push_label(Label::with(id, LabelType::Procedure));

        // Generate the function body
        for instruction in &procedure.body.instructions {
            self.try_generate_instruction(instruction, &mut stack)?;
        }

        // Finally, return
        self.code.push_instruction(agc::Instruction::RETURN, None);

        Ok(WorkspaceArtifact::from(&stack))
    }

    fn try_generate(&mut self, program: &compiler::ProgramCompilation) -> Result<(), GeneratorError> {
        let mut workspace_artifacts: HashMap<ir::Id, WorkspaceArtifact> = HashMap::new();

        // Write out the procedure code
        for (id, proc_def) in &program.procedures {
            let artifact = self.try_generate_procedure(*id, proc_def)?;
            workspace_artifacts.insert(*id, artifact);
        }

        // Write out main
        self.code.push_break();
        self.code.push_raw(format!(
            "MAIN\t\t=\t{}",
            Label {
                id: Id::from(program.main_proc),
                kind: LabelType::Procedure,
            }
        ));

        self.code.push_break();

        // Write the constant pool out
        self.code.push_break();
        self.code.push_comment(String::from("CONSTANTS"));
        self.code.push_break();
        for (id, value) in &program.constants {
            match value {
                ir::ConstValue::Float {
                    base,
                    exponent,
                    precision: ir::Precision::Single,
                } => {
                    self.code
                        .push_raw(format!("{}\tDEC\t{}.0 E{}", Label::with(*id, LabelType::Const), base, exponent));
                }
            }
        }

        // Write the workspace and stack slots out
        self.data.push_comment(String::from("WORKSPACES"));

        for (id, proc) in &program.procedures {
            let workspace_size = proc.layout.placeholders.iter().fold(0, |accum, p| accum + p.words);
            if !proc.layout.placeholders.is_empty() {
                self.data.push_break();
                self.data.push_comment(proc.prototype.signature.clone());
                self.data.push_raw(format!(
                    "{}\t{}\t{}",
                    Label {
                        id: Id::from(*id),
                        kind: LabelType::Workspace
                    },
                    agc::Instruction::ERASE,
                    workspace_size - 1
                ));
            }

            if let Some(artfact) = workspace_artifacts.get(id) {
                if artfact.stack_size > 0 {
                    self.data.push_raw(format!(
                        "{}\t{}\t{}",
                        Label {
                            id: Id::from(*id),
                            kind: LabelType::Stack
                        },
                        agc::Instruction::ERASE,
                        artfact.stack_size - 1
                    ));
                }
            }
        }

        Ok(())
    }
}

impl AssemblyWriter {
    fn new() -> Self {
        Self { buf: Vec::new() }
    }

    fn push_break(&mut self) {
        self.buf.push(AssemblyEntry::Raw(String::from("")));
    }

    fn push_comment(&mut self, comment: String) {
        self.buf.push(AssemblyEntry::Comment(comment));
    }

    fn push_instruction(&mut self, instruction: agc::Instruction, operand: Option<Operand>) {
        self.buf.push(AssemblyEntry::Instruction(instruction, operand));
    }

    fn push_label(&mut self, label: Label) {
        self.buf.push(AssemblyEntry::Label(label));
    }

    fn push_raw(&mut self, s: String) {
        self.buf.push(AssemblyEntry::Raw(s));
    }
}

impl Label {
    fn with(id: ir::Id, kind: LabelType) -> Label {
        Label { id: Id::from(id), kind }
    }
}

impl Generator {
    pub fn try_generate(program: &compiler::ProgramCompilation) -> Result<AssemblyPackage, GeneratorError> {
        let mut code_writer = AssemblyWriter::new();
        let mut data_writer = AssemblyWriter::new();
        let mut generator = AssemblyGenerator::new(&mut code_writer, &mut data_writer);

        generator.try_generate(program)?;

        Ok(AssemblyPackage {
            erasable_source: data_writer.buf,
            fixed_source: code_writer.buf,
        })
    }
}

impl From<ir::Id> for Id {
    fn from(id: ir::Id) -> Self {
        Self(id.0)
    }
}

impl From<ir::Register> for Slot {
    fn from(reg: ir::Register) -> Self {
        Slot {
            label: Label {
                id: Id::from(reg.id),
                kind: LabelType::Workspace,
            },
            offset: reg.offset,
        }
    }
}

impl From<std::io::Error> for GeneratorError {
    fn from(e: std::io::Error) -> Self {
        GeneratorError::IoError(e)
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix: char = match self.kind {
            LabelType::Const => 'C',
            LabelType::Procedure => 'P',
            LabelType::Stack => 'S',
            LabelType::Workspace => 'W',
        };

        write!(f, "{}{:0>7}", prefix, self.id.0)
    }
}

impl std::fmt::Display for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} + {:o}", self.label, self.offset)
    }
}

impl From<Label> for Slot {
    fn from(label: Label) -> Self {
        Slot { label, offset: 0 }
    }
}

impl From<&VirtualStack> for WorkspaceArtifact {
    fn from(vstack: &VirtualStack) -> Self {
        Self {
            stack_size: vstack.maximum_size,
        }
    }
}

impl VirtualStack {
    fn new(id: Id) -> Self {
        Self {
            id,
            offset: 0,
            maximum_size: 0,
        }
    }

    fn push(&mut self) -> Slot {
        let offset = self.offset;
        self.offset += 1;

        self.maximum_size = std::cmp::max(self.offset, self.maximum_size);

        Slot {
            label: Label {
                id: self.id,
                kind: LabelType::Stack,
            },
            offset,
        }
    }

    fn pop(&mut self) -> Slot {
        self.offset -= 1;
        Slot {
            label: Label {
                id: self.id,
                kind: LabelType::Stack,
            },
            offset: self.offset,
        }
    }
}

impl optimizer::Instruction for AssemblyEntry {
    fn is_code_instruction(&self) -> bool {
        match self {
            AssemblyEntry::Instruction(..) => true,
            _ => false,
        }
    }

    fn is_redundant(pair: (&Self, &Self)) -> bool {
        match pair {
            (
                AssemblyEntry::Instruction(agc::Instruction::TS, Some(store_slot)),
                AssemblyEntry::Instruction(agc::Instruction::CAE, Some(load_slot)),
            ) => store_slot == load_slot, // <SLOT> := A followed by A := <SLOT>
            _ => false,
        }
    }
}

impl AssemblyPackage {
    fn flush_labels(f: &mut utils::StringWriter, labels: &mut Vec<Label>) -> Result<bool, WriterError> {
        if let Some((last, rest)) = labels.split_last() {
            // Labels have to be followed by instructions or weird stuff can happen
            // (segfaults in the yaAGC), so we make use of the '=' pseudo op here
            for label in rest {
                write!(f, "{}\t=\t{}\n", label, last)?;
            }

            write!(f, "{}", last)?;
            labels.clear();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn to_assembly_string(source: &Vec<AssemblyEntry>) -> Result<String, WriterError> {
        utils::StringWriter::with(|f| {
            let mut buffered_labels: Vec<Label> = Vec::new();

            for entry in source {
                match entry {
                    AssemblyEntry::Comment(comment) => {
                        write!(f, "# {}\n", comment)?;
                    }
                    AssemblyEntry::Instruction(instr, Some(Operand::Slot(slot))) => {
                        if Self::flush_labels(f, &mut buffered_labels)? {
                            write!(f, "\t")?;
                        } else {
                            write!(f, "\t\t")?;
                        }

                        write!(f, "{}\t{}\n", instr, slot)?;
                    }
                    AssemblyEntry::Instruction(instr, None) => {
                        if Self::flush_labels(f, &mut buffered_labels)? {
                            write!(f, "\t")?;
                        } else {
                            write!(f, "\t\t")?;
                        }

                        write!(f, "{}\n", instr)?;
                    }
                    AssemblyEntry::Label(label) => {
                        buffered_labels.push(*label);
                    }
                    AssemblyEntry::Raw(s) => {
                        write!(f, "{}\n", s)?;
                    }
                }
            }

            Ok(())
        })
    }

    pub fn release_to_yul_assembly(self) -> Result<String, WriterError> {
        utils::StringWriter::with(|f| {
            let raw_fixed_instrs = self.fixed_source;
            let raw_erasable_instrs = self.erasable_source;
            let optimized_fixed_instrs = optimizer::optimize(raw_fixed_instrs);

            let fixed_source_str = Self::to_assembly_string(&optimized_fixed_instrs)?;
            let erasable_source_str = Self::to_assembly_string(&raw_erasable_instrs)?;

            write!(
                f,
                std::include_str!("embed.agc.in"),
                code = &fixed_source_str,
                data = &erasable_source_str,
                entrypoint = "MAIN"
            )
        })
    }
}
