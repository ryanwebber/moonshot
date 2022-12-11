pub struct Instruction {
    opcode: &'static str,
    operands: Vec<String>,
}

impl Instruction {
    pub fn write_opcode<W>(&self, w: &mut W) -> Result<(), std::io::Error> where W: std::io::Write {
        write!(w, "{}", self.opcode)
    }

    pub fn write_operands<W>(&self, w: &mut W) -> Result<(), std::io::Error> where W: std::io::Write {
        for (i, operand) in self.operands.iter().enumerate() {
            if i > 0 {
                write!(w, " ")?;
            }

            write!(w, "{}", operand)?;
        }

        Ok(())
    }
}

impl Instruction {
    fn simple(opcode: &'static str) -> Instruction {
        Instruction {
            opcode,
            operands: Vec::new(),
        }
    }

    fn basic(opcode: &'static str, label: String) -> Instruction {
        Instruction {
            opcode,
            operands: vec![label],
        }
    }
}

/* ----------------- OPCODES ------------------ */

// Noop

pub fn noop() -> Instruction {
    Instruction::simple("NOOP")
}

pub fn tc(label: &str) -> Instruction {
    Instruction::basic("TC", String::from(label))
}

pub fn tc_a() -> Instruction {
    Instruction::simple("XXALQ")
}
