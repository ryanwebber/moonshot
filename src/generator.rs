pub struct Generator;

use std::collections::HashMap;

use crate::agc;
use crate::compiler;
use crate::ir;

struct AssemblyGenerator<'a> {
    writer: AssemblyWriter<'a>,
    slot_allocator: SlotAllocator,
    workspace_registry: WorkspaceRegistry,
}

pub struct AssemblyPackage {
    pub erasable_source: String,
    pub fixed_source: String,
}

struct AssemblyWriter<'a> {
    target: &'a mut dyn SourceWritable,
    queued_labels: Vec<Label>,
}

struct ConstantPool {
    mapping: HashMap<ir::ConstValue, Label>,
}

struct Counter(Id);

pub enum GeneratorError {
    IoError(std::io::Error),
    LabelOverflow,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Id(usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Label {
    id: Id,
    kind: LabelType,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum LabelType {
    Bespoke,
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

struct SlotAllocator {
    constant_pool: ConstantPool,
    bespoke_counter: Counter,
}

trait SourceWritable {
    fn erasable_stream(&mut self) -> &mut dyn std::io::Write;
    fn fixed_stream(&mut self) -> &mut dyn std::io::Write;
}

#[derive(Clone)]
struct Workspace {
    id: Id,
}

struct WorkspaceArtifact {/* information on stack */}

struct WorkspaceContext {
    stack: VirtualStack,
    workspace: Workspace,
}

struct WorkspaceRegistry {
    workspace_mapping: HashMap<Id, Workspace>,
}

struct VirtualStack {}

pub type WriterError = std::io::Error;

impl<'a> AssemblyGenerator<'a> {
    fn new(writer: AssemblyWriter<'a>) -> Self {
        Self {
            writer,
            slot_allocator: SlotAllocator::new(),
            workspace_registry: WorkspaceRegistry::new(),
        }
    }

    fn try_push_const_expr(&mut self, expr: &ir::ConstExpr) -> Result<(), GeneratorError> {
        todo!()
    }

    fn try_push_rval(&mut self, rval: &ir::RVal) -> Result<(), GeneratorError> {
        match rval {
            ir::RVal::Add(expr) => {
                todo!()
            }
            ir::RVal::Const(expr) => {
                todo!()
            }
            ir::RVal::Reg(expr) => {
                todo!()
            }
        }
    }

    fn try_generate_instruction(
        &mut self,
        instruction: &ir::Instruction,
        context: &mut WorkspaceContext,
    ) -> Result<(), GeneratorError> {
        match instruction {
            ir::Instruction::Set(lhs, rhs) => Ok(()),
        }
    }

    fn try_generate_procedure(
        &mut self,
        definition: &compiler::TopLevelDefinition<'_>,
        workspace: &Workspace,
    ) -> Result<WorkspaceArtifact, GeneratorError> {
        let mut context = WorkspaceContext::new(workspace.clone());

        self.writer.write_comment(&format!(
            "{} : {}",
            &definition.module.module_name, &definition.proc.prototype.description
        ))?;

        // Write the function label
        self.writer.push_label(workspace.get_call_slot().label);

        // Generate the function body
        for instruction in &definition.proc.body.instructions {
            self.try_generate_instruction(instruction, &mut context)?;
        }

        // Finally, return
        self.writer
            .write_simple_instruction(agc::Instruction::RETURN, None)?;

        Ok(WorkspaceArtifact::from(&context.stack))
    }

    fn try_generate(
        &mut self,
        program: &compiler::ProgramCompilation<'_>,
    ) -> Result<(), GeneratorError> {
        // Generate workspaces for definitions
        for tld in &program.definitions {
            self.workspace_registry.allocate_workspace(tld.id, tld.proc);
        }

        // Generate code for definitions
        for tld in &program.definitions {
            let workspace = self
                .workspace_registry
                .lookup_workspace(tld.id)
                .expect("Unable to find workspace for procedure")
                .clone();

            self.try_generate_procedure(tld, &workspace)?;
        }

        // In case there are any labels left and to guard agaist falling through to
        // the constants block
        let inf_loop_slot = self.slot_allocator.allocate_bespoke_slot();
        self.writer.write_newline()?;
        self.writer.write_comment("CODE SEGMENT TRAP")?;
        self.writer.push_label(inf_loop_slot.label);
        self.writer
            .write_instruction(agc::Instruction::TC, None, |w| {
                write!(w, "{}", inf_loop_slot)
            })?;

        // Write the constant pool out
        self.writer.write_newline()?;
        self.writer.write_comment("CONSTANTS")?;
        for (value, label) in &self.slot_allocator.constant_pool.mapping {
            match value {
                ir::ConstValue::Float {
                    base,
                    exponent,
                    precision: ir::Precision::Single,
                } => {
                    self.writer.write_const(label, agc::Instruction::DEC, |w| {
                        write!(w, "{}.0 E{}", base, exponent)
                    })?;
                }
            }
        }

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

impl ConstantPool {
    fn new() -> Self {
        Self {
            mapping: HashMap::new(),
        }
    }

    fn get_or_allocate_label(&mut self, value: ir::ConstValue) -> Label {
        if let Some(label) = self.mapping.get(&value) {
            return *label;
        }

        todo!()
    }
}

impl Counter {
    fn post_increment(&mut self) -> Id {
        let r = self.0.clone();
        self.0 .0 += 1;
        r
    }
}

impl Generator {
    pub fn try_generate(
        program: &compiler::ProgramCompilation<'_>,
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

impl From<std::io::Error> for GeneratorError {
    fn from(e: std::io::Error) -> Self {
        GeneratorError::IoError(e)
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let prefix: char = match self.kind {
            LabelType::Bespoke => 'X',
            LabelType::Const => 'C',
            LabelType::Procedure => 'P',
            LabelType::Stack => 'S',
            LabelType::Workspace => 'W',
        };

        write!(f, "{}{:0<7}", prefix, self.id.0)
    }
}

impl std::fmt::Display for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} + {:o}", self.label, self.offset)
    }
}

impl SlotAllocator {
    fn new() -> Self {
        Self {
            constant_pool: ConstantPool::new(),
            bespoke_counter: Counter(Id(0)),
        }
    }

    fn allocate_bespoke_slot(&mut self) -> Slot {
        Slot {
            label: Label {
                id: self.bespoke_counter.post_increment(),
                kind: LabelType::Bespoke,
            },
            offset: 0,
        }
    }

    fn allocate_constant_slot(&mut self, value: ir::ConstValue) -> Slot {
        todo!()
    }
}

impl Workspace {
    fn new(id: Id, proc: &compiler::ProcedureDefinition) -> Self {
        Self {
            id
        }
    }

    fn get_call_slot(&self) -> Slot {
        Slot {
            label: Label {
                id: self.id,
                kind: LabelType::Procedure
            },
            offset: 0,
        }
    }

    fn get_register_slot(&self, register: ir::Register) -> Slot {
        todo!()
    }
}

impl From<&VirtualStack> for WorkspaceArtifact {
    fn from(_: &VirtualStack) -> Self {
        Self { }
    }
}

impl WorkspaceContext {
    fn new(workspace: Workspace) -> Self {
        Self {
            stack: VirtualStack::new(),
            workspace: workspace,
        }
    }
}

impl WorkspaceRegistry {
    fn new() -> Self {
        Self {
            workspace_mapping: HashMap::new(),
        }
    }

    fn allocate_workspace(&mut self, id: ir::Id, proc: &compiler::ProcedureDefinition) {
        let id = Id::from(id);
        let workspace = Workspace::new(id, proc);
        self.workspace_mapping.insert(id, workspace);
    }

    fn lookup_workspace(&mut self, id: ir::Id) -> Option<&Workspace> {
        self.workspace_mapping.get(&Id::from(id))
    }
}

impl VirtualStack {
    fn new() -> Self {
        Self {}
    }

    fn push() -> Slot {
        todo!()
    }

    fn pop() -> Slot {
        todo!()
    }
}
