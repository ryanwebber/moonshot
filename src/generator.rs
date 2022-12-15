pub struct Generator;

use std::collections::HashMap;

use crate::agc;
use crate::compiler;
use crate::ir;

struct AssemblyGenerator<'a> {
    writer: AssemblyWriter<'a>,
}

pub struct AssemblyPackage {
    pub erasable_source: String,
    pub fixed_source: String,
}

struct AssemblyWriter<'a> {
    target: &'a mut dyn SourceWritable,
    queued_labels: Vec<Label>,
}
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

trait SourceWritable {
    fn erasable_stream(&mut self) -> &mut dyn std::io::Write;
    fn fixed_stream(&mut self) -> &mut dyn std::io::Write;
}

struct WorkspaceArtifact {/* information on stack */}

struct WorkspaceContext<'a> {
    procedure: &'a compiler::ProcedureDefinition,
    stack: VirtualStack,
}

struct VirtualStack {
    top: usize,
}

pub type WriterError = std::io::Error;

impl<'a> AssemblyGenerator<'a> {
    fn new(writer: AssemblyWriter<'a>) -> Self {
        Self {
            writer,
        }
    }

    fn try_push_const_expr(&mut self, expr: &ir::ConstExpr) -> Result<(), GeneratorError> {
        let const_slot = Slot::from_const(expr.id);
        self.writer
            .write_instruction(agc::Instruction::CAF, None, |w| {
                write!(w, "{}", const_slot)
            })?;

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
                self.writer.write_comment("TODO: ADD EXPR")?;
                Ok(())
            }
            ir::RVal::Const(expr) => {
                self.try_push_const_expr(expr)?;
                self.writer
                    .write_instruction(agc::Instruction::TS, None, |w| {
                        write!(w, "{}", dest_slot)
                    })?;

                Ok(())
            }
            ir::RVal::Reg(expr) => {
                let source_reg = Slot::from_register(expr.reg);
                self.writer
                    .write_instruction(agc::Instruction::CAE, Some("LOAD REG"), |w| {
                        write!(w, "{}", source_reg)
                    })?;
                
                self.writer
                    .write_instruction(agc::Instruction::TS, None, |w| {
                        write!(w, "{}", dest_slot)
                    })?;
                
                Ok(())
            }
        }
    }

    fn try_pop(&mut self, context: &mut WorkspaceContext) -> Result<Slot, GeneratorError> {
        let slot = context.stack.pop();
        self.writer
            .write_instruction(agc::Instruction::CAE, None, |w| write!(w, "{}", slot))?;

        Ok(slot)
    }

    fn try_generate_instruction(
        &mut self,
        instruction: &ir::Instruction,
        context: &mut WorkspaceContext,
    ) -> Result<(), GeneratorError> {
        match instruction {
            ir::Instruction::Set(lhs, rhs) => {
                let dest_slot = match lhs {
                    ir::LVal::Reg(expr) => Slot::from_register(expr.reg),
                };

                self.try_push(rhs, context)?;
                self.try_pop(context)?;
                self.writer
                    .write_instruction(agc::Instruction::TS, Some("ASSIGN"), |w| {
                        write!(w, "{}", dest_slot)
                    })?;

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
            procedure,
            stack: VirtualStack::new(),
        };

        self.writer.write_comment(&format!("{}", procedure.prototype.signature))?;

        // Write the function label
        self.writer.push_label(Slot::from_procedure(id).label);

        // Generate the function body
        for instruction in &procedure.body.instructions {
            self.try_generate_instruction(instruction, &mut context)?;
        }

        // Finally, return
        self.writer
            .write_simple_instruction(agc::Instruction::RETURN, None)?;

        Ok(WorkspaceArtifact::from(&context.stack))
    }

    fn try_generate(
        &mut self,
        program: &compiler::ProgramCompilation,
    ) -> Result<(), GeneratorError> {

        for (id, proc_def) in &program.procedures {
            self.try_generate_procedure(*id, proc_def)?;
        }

        // Write the constant pool out
        self.writer.write_newline()?;
        self.writer.write_comment("CONSTANTS")?;
        for (id, value) in &program.constants {
            match value {
                ir::ConstValue::Float {
                    base,
                    exponent,
                    precision: ir::Precision::Single,
                } => {
                    let slot = Slot::from_const(*id);
                    self.writer.write_const(&slot.label, agc::Instruction::DEC, |w| {
                        write!(w, "{}.0 E{}", base, exponent)
                    })?;
                }
            }
        }

        // Write the workspace and stack slots out
        self.writer.write_newline()?;
        self.writer.write_comment("WORKSPACE")?;

        Ok(())
    }
}

impl<'a> AssemblyWriter<'a> {
    fn new(target: &'a mut dyn SourceWritable) -> Self {
        Self {
            target,
            queued_labels: Vec::new(),
        }
    }

    fn push_label(&mut self, label: Label) {
        self.queued_labels.push(label);
    }

    fn flush_labels(&mut self) -> Result<bool, WriterError> {
        if let Some((last, rest)) = self.queued_labels.split_last() {
            // Labels have to be followed by instructions or weird stuff can happen
            // (segfaults in the yaAGC). So, we add some NOOP instructions if we have
            // more than one label. This won't won't work for constants or data, so
            // we'll use a different label strategy
            for label in rest {
                write!(self.target.fixed_stream(), "{}\t\tNOOP\n", label)?;
            }

            write!(self.target.fixed_stream(), "{}", last)?;
            self.queued_labels.clear();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn write_const<F>(
        &mut self,
        label: &Label,
        instruction: agc::Instruction,
        f: F,
    ) -> Result<(), WriterError>
    where
        F: FnOnce(&mut dyn std::io::Write) -> Result<(), WriterError>,
    {
        write!(self.target.fixed_stream(), "{}\t", &label)?;
        write!(self.target.fixed_stream(), "{}\t", instruction)?;
        f(self.target.fixed_stream())?;
        write!(self.target.fixed_stream(), "\n")?;

        Ok(())
    }

    fn write_comment(&mut self, c: &str) -> Result<(), WriterError> {
        write!(self.target.fixed_stream(), "# {}\n", c)
    }

    fn write_newline(&mut self) -> Result<(), WriterError> {
        write!(self.target.fixed_stream(), "\n")
    }

    fn write_instruction<F>(
        &mut self,
        instruction: agc::Instruction,
        comment: Option<&str>,
        f: F,
    ) -> Result<(), WriterError>
    where
        F: FnOnce(&mut dyn std::io::Write) -> Result<(), WriterError>,
    {
        if self.flush_labels()? {
            write!(self.target.fixed_stream(), "\t")?;
        } else {
            write!(self.target.fixed_stream(), "\t\t")?;
        }

        write!(self.target.fixed_stream(), "{}\t", instruction)?;
        f(self.target.fixed_stream())?;

        if let Some(comment) = comment {
            write!(self.target.fixed_stream(), "\t")?;
            write!(self.target.fixed_stream(), "# {}", comment)?;
        }

        write!(self.target.fixed_stream(), "\n")?;

        Ok(())
    }

    fn write_simple_instruction(
        &mut self,
        instruction: agc::Instruction,
        comment: Option<&str>,
    ) -> Result<(), WriterError> {
        if self.flush_labels()? {
            write!(self.target.fixed_stream(), "\t")?;
        } else {
            write!(self.target.fixed_stream(), "\t\t")?;
        }

        write!(self.target.fixed_stream(), "{}", instruction)?;

        if let Some(comment) = comment {
            write!(self.target.fixed_stream(), "\t")?;
            write!(self.target.fixed_stream(), "# {}", comment)?;
        }

        write!(self.target.fixed_stream(), "\n")?;

        Ok(())
    }
}

impl Generator {
    pub fn try_generate(
        program: &compiler::ProgramCompilation,
    ) -> Result<AssemblyPackage, GeneratorError> {
        struct InlineTarget<'a> {
            erasable_buf: &'a mut Vec<u8>,
            fixed_buf: &'a mut Vec<u8>,
        }

        impl<'a> SourceWritable for InlineTarget<'a> {
            fn erasable_stream(&mut self) -> &mut dyn std::io::Write {
                self.erasable_buf
            }

            fn fixed_stream(&mut self) -> &mut dyn std::io::Write {
                self.fixed_buf
            }
        }

        let mut erasable_buf = Vec::new();
        let mut fixed_buf = Vec::new();

        let mut target = InlineTarget {
            erasable_buf: &mut erasable_buf,
            fixed_buf: &mut fixed_buf,
        };

        let writer = AssemblyWriter::new(&mut target);
        let mut generator = AssemblyGenerator::new(writer);

        generator.try_generate(program)?;

        let erasable_source = std::str::from_utf8(erasable_buf.as_slice())
            .expect("Unable to unwrap buffer as string")
            .to_string();

        let fixed_source = std::str::from_utf8(fixed_buf.as_slice())
            .expect("Unable to unwrap buffer as string")
            .to_string();

        Ok(AssemblyPackage {
            erasable_source,
            fixed_source,
        })
    }
}

impl From<ir::Id> for Id {
    fn from(id: ir::Id) -> Self {
        Self(id.0)
    }
}

impl From<ir::Register> for Id {
    fn from(id: ir::Register) -> Self {
        Self(id.0)
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

impl Slot {
    fn from_const(id: ir::Id) -> Slot {
        Slot {
            label: Label {
                id: Id::from(id),
                kind: LabelType::Const,
            },
            offset: 0
        }
    }

    fn from_procedure(id: ir::Id) -> Slot {
        Slot {
            label: Label {
                id: Id::from(id),
                kind: LabelType::Procedure,
            },
            offset: 0
        }
    }

    fn from_register(id: ir::Register) -> Slot {
        Slot {
            label: Label {
                id: Id::from(id),
                kind: LabelType::Workspace,
            },
            offset: 0
        }
    }
}

impl From<&VirtualStack> for WorkspaceArtifact {
    fn from(_: &VirtualStack) -> Self {
        Self {}
    }
}

impl<'a> WorkspaceContext<'a> {
    
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
