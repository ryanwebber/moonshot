use std::fmt::Display;

use crate::{compiler::Label, types::Numeric};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Instruction {
    AD,
    ADS,
    CAE(Address),
    CAF(Address),
    DEC(Numeric),
    ERASE,
    NOOP,
    QXCH(Address),
    RETURN,
    TC,
    TCA,
    TS,
    XCH(Address),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            AD => write!(f, "AD"),
            ADS => write!(f, "ADS"),
            CAE(address) => write!(f, "CAE\t{}", address),
            CAF(address) => write!(f, "CAF\t{}", address),
            DEC(value) => write!(f, "DEC\t{}", value),
            ERASE => write!(f, "ERASE"),
            NOOP => write!(f, "NOOP"),
            QXCH(address) => write!(f, "QXCH\t{}", address),
            RETURN => write!(f, "RETURN"),
            TC => write!(f, "TC"),
            TCA => write!(f, "TCA"),
            TS => write!(f, "TS"),
            XCH(address) => write!(f, "XCH\t{}", address),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Address {
    Relative { label: Label, offset: i32 },
}

impl Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Address::*;
        match self {
            Relative { label, offset } => match *offset {
                0 => write!(f, "{}", label),
                offset if offset < 0 => write!(f, "{} - {}", label, -offset),
                offset => write!(f, "{} + {}", label, offset),
            },
        }
    }
}
