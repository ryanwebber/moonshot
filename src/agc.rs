pub struct Instruction {
    opcode: &'static str,
    operands: Vec<String>,
}

impl Instruction {
    pub fn write_opcode(&self, w: &mut dyn std::io::Write) -> Result<(), std::io::Error> {
        write!(w, "{}", self.opcode)
    }

    pub fn write_operands(&self, w: &mut dyn std::io::Write) -> Result<(), std::io::Error> {
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

pub fn ins_noop() -> Instruction {
    Instruction::simple("NOOP")
}

pub fn ins_return() -> Instruction {
    Instruction::simple("RETURN")
}

pub fn ins_tc(label: &str) -> Instruction {
    Instruction::basic("TC", String::from(label))
}

pub fn ins_tc_a() -> Instruction {
    Instruction::simple("XXALQ")
}
