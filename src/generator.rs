
pub struct Generator;

use std::collections::HashMap;
use std::collections::HashSet;

use crate::agc;
use crate::compiler;

type WriterError = std::io::Error;

pub struct GeneratorError {
}

struct AssemblyWriter<'a, T> where T: std::io::Write {
    writer: &'a mut T,
    queued_labels: Vec<String>,
}

struct LabelGenerator {
    next_module_index: u32,
    next_procedure_index: u32,
    module_prefixes: HashMap<String, u32>,
    procedure_prefixes: HashMap<String, u32>,
}

struct LabelId {
    module_id: u32,
    procedure_id: u32,
}

impl From<std::io::Error> for GeneratorError {
    fn from(_: std::io::Error) -> Self {
        GeneratorError {  }
    }
}

impl<'a, T> AssemblyWriter<'a, T> where T: std::io::Write {

    fn new(writer: &'a mut T) -> Self {
        Self {
            writer,
            queued_labels: Vec::new(),
        }
    }

    fn push_label(&mut self, label: String) {
        self.queued_labels.push(label);
    }

    fn flush_labels(&mut self) -> Result<Option<usize>, WriterError> {
        if let Some((last, rest)) = self.queued_labels.split_last() {
            for label in rest {
                write!(self.writer, "{}\t\tNOOP\n", label)?;
            }

            write!(self.writer, "{}", last)?;
            let len = last.len();
            self.queued_labels.clear();
            Ok(Some(len))
        } else {
            Ok(None)
        }
    }

    fn write_comment(&mut self, c: &str) -> Result<(), WriterError> {
        write!(self.writer, "# {}\n", c)
    }

    fn write_line(&mut self, instruction: agc::Instruction, comment: Option<&str>) -> Result<(), WriterError> {
        match self.flush_labels()? {
            Some(len) if len >= 8 => {
                write!(self.writer, "\t")?;
            }
            _ => {
                write!(self.writer, "\t\t")?;
            }
        };

        instruction.write_opcode(self.writer)?;
        write!(self.writer, "\t")?;
        instruction.write_operands(self.writer)?;

        if let Some(comment) = comment {
            write!(self.writer, "\t")?;
            write!(self.writer, "# {}", comment)?;
        }

        write!(self.writer, "\n")?;

        Ok(())
    }
}

impl Generator {
    pub fn try_generate<T>(program: &compiler::ProgramCompilation<'_>, writer: &mut T) -> Result<(), GeneratorError> where T: std::io::Write {
        let mut w = AssemblyWriter::new(writer);
        let mut label_generator = LabelGenerator::new();

        for module in program.modules {
            for proc in &module.procedures {
                let label_id = label_generator.get_or_allocate(&module.module_name, &proc.prototype.signature);
                w.write_comment(&format!("{} : {}", module.module_name, proc.prototype.description))?;
                w.push_label(label_id.format().iter().collect());
                for _ in &proc.body.instructions {
                    w.write_line(agc::noop(), None)?;
                }
            }
        }

        Ok(())
    }
}

impl LabelId {
    fn format(&self) -> [char; 8] {
        let mut result: [char; 8] = ['A'; 8];
    
        let mut m_id = self.module_id;
        for i in 0..=2 {
            let (div, rem) = (m_id / 26, m_id % 26);
            result[2 - i] = (('A' as u8) + (rem as u8)) as u8 as char;
            m_id = div;
        }

        let mut p_id = self.procedure_id;
        for i in 0..=3 {
            let (div, rem) = (p_id / 26, p_id % 26);
            result[7 - i] = (('A' as u8) + (rem as u8)) as u8 as char;
            p_id = div;
        }

        result[3] = '$';

        result
    }
}

impl LabelGenerator {
    fn new() -> LabelGenerator {
        LabelGenerator {
            next_module_index: 0,
            next_procedure_index: 0,
            module_prefixes: HashMap::new(),
            procedure_prefixes: HashMap::new(),
        }
    }

    fn get_or_allocate(&mut self, module: &str, signature: &str) -> LabelId {
        let module_id = match self.module_prefixes.get(module) {
            Some(idx) => *idx,
            _ => {
                let idx = self.next_module_index;
                self.module_prefixes.insert(String::from(module), idx);
                self.next_module_index += 1;
                idx
            }
        };

        let procedure_id = match self.procedure_prefixes.get(signature) {
            Some(idx) => *idx,
            _ => {
                let idx = self.next_procedure_index;
                self.procedure_prefixes.insert(String::from(module), idx);
                self.next_procedure_index += 1;
                idx
            }
        };

        LabelId { module_id, procedure_id, }
    }
}
