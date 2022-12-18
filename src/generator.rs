use crate::agc;
use crate::compiler;
use crate::ir;
use crate::utils;

struct AssemblyGenerator<'a> {
    code: AssemblyWriter<'a>,
    data: AssemblyWriter<'a>,
}

pub struct AssemblyPackage {
    pub erasable_source: String,
    pub fixed_source: String,
}

struct AssemblyWriter<'a> {
    w: &'a mut dyn std::io::Write,
    queued_labels: Vec<Label>,
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
    fn new(code_writer: AssemblyWriter<'a>, data_writer: AssemblyWriter<'a>) -> Self {
        Self {
            code: code_writer,
            data: data_writer,
        }
    }

    fn try_push_const_expr(&mut self, expr: &ir::ConstExpr) -> Result<(), GeneratorError> {
        let const_slot: Slot = Label::with(expr.id, LabelType::Const).into();
        self.code
            .write_instruction_with(agc::Instruction::CAF, None, |w| write!(w, "{}", const_slot))?;

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
                    .write_instruction_with(agc::Instruction::ADS, None, |w| {
                        write!(w, "{}", dest_slot)
                    })?;

                context.stack.top -= 1;

                Ok(())
            }
            ir::RVal::Const(expr) => {
                self.try_push_const_expr(expr)?;
                self.code
                    .write_instruction_with(agc::Instruction::TS, None, |w| {
                        write!(w, "{}", dest_slot)
                    })?;

                Ok(())
            }
            ir::RVal::Reg(expr) => {
                let source_reg = Slot::from(expr.reg);
                self.code.write_instruction_with(
                    agc::Instruction::CAE,
                    Some(&format!("A := R{}[{}]", expr.reg.id, expr.reg.offset)),
                    |w| write!(w, "{}", source_reg),
                )?;

                self.code
                    .write_instruction_with(agc::Instruction::TS, None, |w| {
                        write!(w, "{}", dest_slot)
                    })?;

                Ok(())
            }
        }
    }

    fn try_pop(&mut self, context: &mut WorkspaceContext) -> Result<Slot, GeneratorError> {
        let slot = context.stack.pop();
        self.code
            .write_instruction_with(agc::Instruction::CAE, None, |w| write!(w, "{}", slot))?;

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
                self.code.write_instruction_with(
                    agc::Instruction::TS,
                    Some(&format!("R{}[{}] := A", register.id, register.offset)),
                    |w| write!(w, "{}", dest_slot),
                )?;

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
            .write_comment_with(|w| write!(w, "{}", procedure.prototype.signature))?;

        // Write the function label
        self.code.push_label(Label::with(id, LabelType::Procedure));

        // Generate the function body
        for instruction in &procedure.body.instructions {
            self.try_generate_instruction(instruction, &mut context)?;
        }

        // Finally, return
        self.code
            .write_instruction(agc::Instruction::RETURN, None)?;

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
        self.code.write_newline()?;
        write!(
            self.code.w,
            "MAIN\t\t=\t{}\n",
            Label {
                id: Id::from(program.main_proc),
                kind: LabelType::Procedure
            }
        )?;

        self.code.write_newline()?;

        // Write the constant pool out
        self.code.write_newline()?;
        self.code.write_comment("CONSTANTS")?;
        self.code.write_newline()?;
        for (id, value) in &program.constants {
            match value {
                ir::ConstValue::Float {
                    base,
                    exponent,
                    precision: ir::Precision::Single,
                } => {
                    self.code.push_label(Label::with(*id, LabelType::Const));
                    self.code
                        .write_instruction_with(agc::Instruction::DEC, None, |w| {
                            write!(w, "{}.0 E{}", base, exponent)
                        })?;
                }
            }
        }

        // Write the workspace and stack slots out
        self.data.write_comment("WORKSPACES")?;
        self.data.write_newline()?;

        for (_, proc) in &program.procedures {
            if !proc.layout.placeholders.is_empty() {
                self.data
                    .write_comment_with(|w| write!(w, "{}", proc.prototype.signature))?;
                for placeholder in &proc.layout.placeholders {
                    self.data.push_label(Label {
                        id: Id::from(placeholder.id),
                        kind: LabelType::Workspace,
                    });

                    self.data.write_instruction_with(
                        agc::Instruction::ERASE,
                        Some(&format!("R{}", placeholder.id)),
                        |w| write!(w, "{}", placeholder.words - 1),
                    )?;
                }
            }
        }

        Ok(())
    }
}

impl<'a> AssemblyWriter<'a> {
    fn new(w: &'a mut dyn std::io::Write) -> Self {
        Self {
            w,
            queued_labels: Vec::new(),
        }
    }

    fn push_label(&mut self, label: Label) {
        self.queued_labels.push(label);
    }

    fn flush_labels(&mut self) -> Result<bool, WriterError> {
        if let Some((last, rest)) = self.queued_labels.split_last() {
            // Labels have to be followed by instructions or weird stuff can happen
            // (segfaults in the yaAGC), so we make use of the '=' pseudo op here
            for label in rest {
                write!(self.w, "{}\t=\t{}\n", label, last)?;
            }

            write!(self.w, "{}", last)?;
            self.queued_labels.clear();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn write_comment(&mut self, c: &str) -> Result<(), WriterError> {
        write!(self.w, "# {}\n", c)
    }

    fn write_comment_with<F>(&mut self, f: F) -> Result<(), WriterError>
    where
        F: FnOnce(&mut dyn std::io::Write) -> Result<(), WriterError>,
    {
        write!(self.w, "# ")?;
        f(self.w)?;
        write!(self.w, "\n")?;
        Ok(())
    }

    fn write_newline(&mut self) -> Result<(), WriterError> {
        write!(self.w, "\n")
    }

    fn write_instruction_with<F>(
        &mut self,
        instruction: agc::Instruction,
        comment: Option<&str>,
        f: F,
    ) -> Result<(), WriterError>
    where
        F: FnOnce(&mut dyn std::io::Write) -> Result<(), WriterError>,
    {
        if self.flush_labels()? {
            write!(self.w, "\t")?;
        } else {
            write!(self.w, "\t\t")?;
        }

        write!(self.w, "{}\t", instruction)?;
        f(self.w)?;

        if let Some(comment) = comment {
            write!(self.w, "\t")?;
            write!(self.w, "# {}", comment)?;
        }

        write!(self.w, "\n")?;

        Ok(())
    }

    fn write_instruction(
        &mut self,
        instruction: agc::Instruction,
        comment: Option<&str>,
    ) -> Result<(), WriterError> {
        if self.flush_labels()? {
            write!(self.w, "\t")?;
        } else {
            write!(self.w, "\t\t")?;
        }

        write!(self.w, "{}", instruction)?;

        if let Some(comment) = comment {
            write!(self.w, "\t")?;
            write!(self.w, "# {}", comment)?;
        }

        write!(self.w, "\n")?;

        Ok(())
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
        let mut erasable_buf = utils::StringWriter::new();
        let mut fixed_buf = utils::StringWriter::new();

        let code_writer = AssemblyWriter::new(&mut fixed_buf);
        let data_writer = AssemblyWriter::new(&mut erasable_buf);
        let mut generator = AssemblyGenerator::new(code_writer, data_writer);

        generator.try_generate(program)?;

        let erasable_source = erasable_buf
            .as_str()
            .expect("Unable to unwrap buffer as string")
            .to_string();

        let fixed_source = fixed_buf
            .as_str()
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

impl AssemblyPackage {
    pub fn to_yul_assembly(&self) -> Result<String, WriterError> {
        utils::StringWriter::with(|f| {
            use std::io::Write;
            write!(
                f,
                std::include_str!("embed.agc.in"),
                code = &self.fixed_source,
                data = &self.erasable_source,
                entrypoint = "MAIN"
            )
        })
    }
}
