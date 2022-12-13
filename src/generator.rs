pub struct Generator;

use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::agc;
use crate::compiler;
use crate::ir;

struct AssemblyGenerator<'a> {
    writer: AssemblyWriter<'a>,
    label_generator: LabelGenerator,
    slot_allocator: SlotAllocator,
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Label(Rc<String>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum LabelTarget {
    Procedure { module: String, procedure: String },
}

struct LabelGenerator {
    all_labels: HashSet<Label>,
    lookup_table: HashMap<LabelTarget, Label>,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Register {
    A = 0o0,
    L = 0o1,
    Q = 0o2,
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
enum Slot {
    Addressed(u32),
    Labelled(Label),
}

struct SlotAllocator {
    mapping: HashMap<ir::Register, Slot>,
}

trait SourceWritable {
    fn erasable_stream(&mut self) -> &mut dyn std::io::Write;
    fn fixed_stream(&mut self) -> &mut dyn std::io::Write;
}

pub type WriterError = std::io::Error;

impl<'a> AssemblyGenerator<'a> {
    fn new(writer: AssemblyWriter<'a>) -> Self {
        Self {
            writer,
            label_generator: LabelGenerator::new(),
            slot_allocator: SlotAllocator::new(),
        }
    }

    fn try_generate_rval(&mut self, rval: &ir::RVal) -> Result<Slot, GeneratorError> {
        match rval {
            ir::RVal::Add(expr) => {
                Ok(Slot::from(Register::A))
            }
            ir::RVal::Const(expr) => match expr.value {
                ir::ConstValue::Float {
                    data,
                    precision: ir::Precision::Single,
                } => {
                    Ok(Slot::from(Register::A))
                }
            },
            ir::RVal::Reg(expr) => {
                let slot = self
                    .slot_allocator
                    .get(&expr.reg)
                    .expect("Register has no known location");

                Ok(slot.clone())
            }
        }
    }

    fn try_generate_instruction(
        &mut self,
        instruction: &ir::Instruction,
    ) -> Result<(), GeneratorError> {
        match instruction {
            ir::Instruction::Set(lhs, rhs) => {
                self.writer.write_line(agc::ins_noop(), None)?;
                let slot = self.try_generate_rval(rhs)?.clone();
                match lhs {
                    ir::LVal::Reg(expr) => {
                        // Claim the slot the rval is in for the lval register
                        self.slot_allocator.claim(slot, expr.reg)
                    }
                }
            }
        };

        Ok(())
    }

    fn try_generate_procedure(
        &mut self,
        proc: &compiler::ProcedureDefinition,
        module: &compiler::ModuleCompilation,
    ) -> Result<(), GeneratorError> {
        let label = {
            let label_target = LabelTarget::Procedure {
                module: module.module_name.to_string(),
                procedure: proc.prototype.name.to_string(),
            };

            self.label_generator
                .get_or_allocate(&label_target)
                .map_err(|_| GeneratorError::LabelOverflow)?
        };

        self.writer.write_comment(&format!(
            "{} : {}",
            module.module_name, proc.prototype.description
        ))?;

        self.writer.push_label(label);

        // Generate the function body
        for instruction in &proc.body.instructions {
            self.try_generate_instruction(instruction)?;
        }

        // Finally, return
        self.writer.write_line(agc::ins_return(), None)?;

        Ok(())
    }

    fn try_generate(
        &mut self,
        program: &compiler::ProgramCompilation<'_>,
    ) -> Result<(), GeneratorError> {
        for module in program.modules {
            for proc in &module.procedures {
                self.try_generate_procedure(proc, module)?;
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

    fn flush_labels(&mut self) -> Result<Option<usize>, WriterError> {
        if let Some((last, rest)) = self.queued_labels.split_last() {
            // Labels have to be followed by instructions or weird stuff can happen
            // (segfaults in the yaAGC). So, we add some NOOP instructions if we have
            // more than one label. This won't won't work for constants or data, so
            // we'll use a different label strat
            for label in rest {
                write!(self.target.fixed_stream(), "{}\t\tNOOP\n", label.as_str())?;
            }

            write!(self.target.fixed_stream(), "{}", last.as_str())?;
            let len = last.0.len();
            self.queued_labels.clear();
            Ok(Some(len))
        } else {
            Ok(None)
        }
    }

    fn write_comment(&mut self, c: &str) -> Result<(), WriterError> {
        write!(self.target.fixed_stream(), "# {}\n", c)
    }

    fn write_line(
        &mut self,
        instruction: agc::Instruction,
        comment: Option<&str>,
    ) -> Result<(), WriterError> {
        match self.flush_labels()? {
            Some(len) if len >= 8 => {
                write!(self.target.fixed_stream(), "\t")?;
            }
            _ => {
                write!(self.target.fixed_stream(), "\t\t")?;
            }
        };

        instruction.write_opcode(self.target.fixed_stream())?;
        write!(self.target.fixed_stream(), "\t")?;
        instruction.write_operands(self.target.fixed_stream())?;

        if let Some(comment) = comment {
            write!(self.target.fixed_stream(), "\t")?;
            write!(self.target.fixed_stream(), "# {}", comment)?;
        }

        write!(self.target.fixed_stream(), "\n")?;

        Ok(())
    }
}

impl Generator {
    pub fn try_generate(program: &compiler::ProgramCompilation<'_>) -> Result<AssemblyPackage, GeneratorError> {
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
            erasable_source, fixed_source,
        })
    }
}

impl From<std::io::Error> for GeneratorError {
    fn from(e: std::io::Error) -> Self {
        GeneratorError::IoError(e)
    }
}

impl Label {
    fn as_str<'a>(&'a self) -> &'a str {
        &self.0
    }
}

impl LabelGenerator {
    fn new() -> Self {
        Self {
            all_labels: HashSet::new(),
            lookup_table: HashMap::new(),
        }
    }

    fn get_or_allocate(&mut self, target: &LabelTarget) -> Result<Label, ()> {
        let ideal_name = target.ideal_name();

        // TODO: This can be more efficient with fewer allocations
        for i in 0..=999 {
            let num_str = format!("{}", i);
            let prefix_len = ideal_name.len() - num_str.len();
            let label_attempt = Label(Rc::new(format!("{}{}", &ideal_name[..prefix_len], num_str)));
            if !self.all_labels.contains(&label_attempt) {
                self.all_labels.insert(label_attempt.clone());
                self.lookup_table
                    .insert(target.clone(), label_attempt.clone());

                return Ok(label_attempt);
            }
        }

        Err(())
    }
}

impl LabelTarget {
    fn ideal_name(&self) -> String {
        match self {
            Self::Procedure { module, procedure } => {
                let mod_name_len = std::cmp::min(module.len(), 3);
                let mod_name_component = &module[..mod_name_len];

                let max_proc_name_len = 7 - mod_name_len;
                let proc_name_len = std::cmp::min(max_proc_name_len, procedure.len());
                let proc_name_component = &procedure[..proc_name_len];

                format!(
                    "{}${:%<width$}",
                    mod_name_component,
                    proc_name_component,
                    width = max_proc_name_len
                )
            }
        }
    }
}

impl From<Register> for Slot {
    fn from(register: Register) -> Self {
        Slot::Addressed(register as u32)
    }
}

impl PartialEq<Register> for Slot {
    fn eq(&self, other: &Register) -> bool {
        Slot::from(*other) == *self
    }
}

impl SlotAllocator {
    fn new() -> Self {
        Self {
            mapping: HashMap::new(),
        }
    }

    fn claim(&mut self, slot: Slot, register: ir::Register) {
        self.mapping.insert(register, slot);
    }

    fn get(&self, register: &ir::Register) -> Option<&Slot> {
        self.mapping.get(register)
    }
}

// Takeaways
// - Constants go into fixed mem
// - Constants need to be pooled (limited fixed mem)
// - Workspace stuff goes into eraseable mem
// - OCTALS are everywhere
// - Need different write streams for fixed/erasable memory
