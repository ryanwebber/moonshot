use std::fmt::Display;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub enum Instruction {
    AD,
    ADS,
    CAE,
    CAF,
    DEC,
    ERASE,
    NOOP,
    RETURN,
    TC,
    TCA,
    TS,
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            AD => write!(f, "AD"),
            ADS => write!(f, "ADS"),
            CAE => write!(f, "CAE"),
            CAF => write!(f, "CAF"),
            DEC => write!(f, "DEC"),
            ERASE => write!(f, "ERASE"),
            NOOP => write!(f, "NOOP"),
            RETURN => write!(f, "RETURN"),
            TC => write!(f, "TC"),
            TCA => write!(f, "TCA"),
            TS => write!(f, "TS"),
        }
    }
}
