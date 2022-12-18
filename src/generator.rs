use crate::agc;
use crate::compiler;
use crate::ir;
use crate::optimizer;
use crate::utils;

use std::io::Write;

enum AssemblyEntry {
    Comment(String),
    Instruction(agc::Instruction, Option<Slot>),
    Label(Label),
    Raw(Vec<String>),
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

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
struct Slot {
    label: Label,
    offset: usize,
}

struct WorkspaceArtifact {/* information on stack */}

struct WorkspaceContext {
    stack: VirtualStack,
}

struct VirtualStack {
    top: usize,
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
        self.code
            .push_instruction(agc::Instruction::CAF, Some(const_slot));
        Ok(())
    }

    fn try_push(
        &mut self,
        rval: &ir::RVal,
        context: &mut WorkspaceContext,
    ) -> Result<(), GeneratorError> {
        let dest_slot = context.stack.push();
        match rval {
            ir::RVal::Add(expr) => {
                self.try_push(&*expr.lhs, context)?;
                self.try_push(&*expr.rhs, context)?;
                self.try_pop(context)?;
                self.code
                    .push_instruction(agc::Instruction::ADS, Some(dest_slot));
                context.stack.top -= 1;

                Ok(())
            }
            ir::RVal::Const(expr) => {
                self.try_push_const_expr(expr)?;
                self.code
                    .push_instruction(agc::Instruction::TS, Some(dest_slot));
                Ok(())
            }
            ir::RVal::Reg(expr) => {
                let source_slot = Slot::from(expr.reg);
                self.code
                    .push_instruction(agc::Instruction::CAE, Some(source_slot));
                self.code
                    .push_instruction(agc::Instruction::TS, Some(dest_slot));
                Ok(())
            }
        }
    }

    fn try_pop(&mut self, context: &mut WorkspaceContext) -> Result<Slot, GeneratorError> {
        let slot = context.stack.pop();
        self.code
            .push_instruction(agc::Instruction::CAE, Some(slot.clone()));
        Ok(slot)
    }

    fn try_generate_instruction(
        &mut self,
        instruction: &ir::Instruction,
        context: &mut WorkspaceContext,
    ) -> Result<(), GeneratorError> {
        match instruction {
            ir::Instruction::Set(lhs, rhs) => {
                let register = match lhs {
                    ir::LVal::Reg(expr) => expr.reg,
                };

                let dest_slot = Slot::from(register);
                self.try_push(rhs, context)?;
                self.try_pop(context)?;
                self.code
                    .push_instruction(agc::Instruction::TS, Some(dest_slot));

                Ok(())
            }
        }
    }

    fn try_generate_procedure(
        &mut self,
        id: ir::Id,
        procedure: &compiler::ProcedureDefinition,
    ) -> Result<WorkspaceArtifact, GeneratorError> {
        let mut context = WorkspaceContext {
            stack: VirtualStack::new(),
        };

        self.code
            .push_comment(format!("{}", procedure.prototype.signature));

        // Write the function label
        self.code.push_label(Label::with(id, LabelType::Procedure));

        // Generate the function body
        for instruction in &procedure.body.instructions {
            self.try_generate_instruction(instruction, &mut context)?;
        }

        // Finally, return
        self.code.push_instruction(agc::Instruction::RETURN, None);

        Ok(WorkspaceArtifact::from(&context.stack))
    }

    fn try_generate(
        &mut self,
        program: &compiler::ProgramCompilation,
    ) -> Result<(), GeneratorError> {
        // Write out the procedure code
        for (id, proc_def) in &program.procedures {
            self.try_generate_procedure(*id, proc_def)?;
        }

        // Write out main
        self.code.push_break();
        self.code.push_columns(vec![
            String::from("MAIN"),
            String::from("="),
            Label {
                id: Id::from(program.main_proc),
                kind: LabelType::Procedure,
            }
            .to_string(),
        ]);

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
                    self.code.push_columns(vec![
                        Label::with(*id, LabelType::Const).to_string(),
                        String::from("DEC"),
                        format!("{}.0 E{}", base, exponent),
                    ]);
                }
            }
        }

        // Write the workspace and stack slots out
        self.data.push_comment(String::from("WORKSPACES"));
        self.data.push_break();

        for (_, proc) in &program.procedures {
            if !proc.layout.placeholders.is_empty() {
                self.data.push_comment(proc.prototype.signature.clone());
                for placeholder in &proc.layout.placeholders {
                    self.data.push_columns(vec![
                        Label {
                            id: Id::from(placeholder.id),
                            kind: LabelType::Workspace,
                        }
                        .to_string(),
                        agc::Instruction::ERASE.to_string(),
                        format!("{}", placeholder.words - 1),
                    ]);
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
        self.buf.push(AssemblyEntry::Raw(Vec::new()));
    }

    fn push_comment(&mut self, comment: String) {
        self.buf.push(AssemblyEntry::Comment(comment));
    }

    fn push_columns(&mut self, columns: Vec<String>) {
        self.buf.push(AssemblyEntry::Raw(columns));
    }

    fn push_instruction(&mut self, instruction: agc::Instruction, operand: Option<Slot>) {
        self.buf
            .push(AssemblyEntry::Instruction(instruction, operand));
    }

    fn push_label(&mut self, label: Label) {
        self.buf.push(AssemblyEntry::Label(label));
    }
}

impl Label {
    fn with(id: ir::Id, kind: LabelType) -> Label {
        Label {
            id: Id::from(id),
            kind,
        }
    }
}

impl Generator {
    pub fn try_generate(
        program: &compiler::ProgramCompilation,
    ) -> Result<AssemblyPackage, GeneratorError> {
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
    fn from(_: &VirtualStack) -> Self {
        Self {}
    }
}

impl VirtualStack {
    fn new() -> Self {
        Self { top: 0 }
    }

    fn push(&mut self) -> Slot {
        let offset = self.top;
        self.top += 1;
        Slot {
            label: Label {
                id: Id(0),
                kind: LabelType::Stack,
            },
            offset,
        }
    }

    fn pop(&mut self) -> Slot {
        self.top -= 1;
        Slot {
            label: Label {
                id: Id(0),
                kind: LabelType::Stack,
            },
            offset: self.top,
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
                AssemblyEntry::Instruction(agc::Instruction::TS, store_slot),
                AssemblyEntry::Instruction(agc::Instruction::CAE, load_slot),
            ) => store_slot == load_slot, // <SLOT> := A followed by A := <SLOT>
            _ => false,
        }
    }
}

impl AssemblyPackage {
    fn flush_labels(
        f: &mut utils::StringWriter,
        labels: &mut Vec<Label>,
    ) -> Result<bool, WriterError> {
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
                    AssemblyEntry::Instruction(instr, Some(slot)) => {
                        if Self::flush_labels(f, &mut buffered_labels)? {
                            write!(f, "\t")?;
                        } else {
                            write!(f, "\t\t")?;
                        }

                        write!(f, "{}\t{}\n", instr, slot)?;
                    }
                    AssemblyEntry::Instruction(instr, ..) => {
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
                    AssemblyEntry::Raw(columns) => {
                        let tabs = if let Some(label) = columns.first() {
                            write!(f, "{}", label)?;
                            if label.len() < 8 {
                                2
                            } else {
                                1
                            }
                        } else {
                            2
                        };

                        if let Some(instruction) = columns.get(1) {
                            for _ in 0..tabs {
                                write!(f, "\t")?;
                            }

                            write!(f, "{}", instruction)?;
                        }

                        if columns.len() > 2 {
                            write!(f, "\t")?;
                            for (i, operand) in columns.iter().skip(2).enumerate() {
                                if i > 0 {
                                    write!(f, " ")?;
                                }

                                write!(f, "{}", operand)?;
                            }
                        }

                        write!(f, "\n")?;
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
