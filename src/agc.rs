use std::fmt::Display;

use crate::{generator::Label, types::Numeric};

#[derive(Debug, PartialEq, Clone, Eq, Hash)]
pub enum Instruction {
    ADS(Address),
    CAE(Address),
    CAF(Address),
    DEC(Numeric),
    ERASE,
    NOOP,
    QXCH(Address),
    RETURN,
    TC(Address),
    XCH(Address),
}

impl Instruction {
    pub fn is_extend(&self) -> bool {
        match self {
            Instruction::QXCH(..) => true,
            _ => false,
        }
    }

    pub fn from_str_with_address(instruction: &str, address: Address) -> Option<Self> {
        use Instruction::*;
        Some(match instruction {
            "ADS" => ADS(address),
            "CAE" => CAE(address),
            "CAF" => CAF(address),
            "QXCH" => QXCH(address),
            "TC" => TC(address),
            "XCH" => XCH(address),
            _ => return None,
        })
    }

    pub fn from_str(instruction: &str) -> Option<Self> {
        use Instruction::*;
        Some(match instruction {
            "ERASE" => ERASE,
            "NOOP" => NOOP,
            "RETURN" => RETURN,
            _ => return None,
        })
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Instruction::*;
        match self {
            ADS(address) => write!(f, "ADS\t{}", address),
            CAE(address) => write!(f, "CAE\t{}", address),
            CAF(address) => write!(f, "CAF\t{}", address),
            DEC(value) => write!(f, "DEC\t{}", value),
            ERASE => write!(f, "ERASE"),
            NOOP => write!(f, "NOOP"),
            QXCH(address) => write!(f, "QXCH\t{}", address),
            RETURN => write!(f, "RETURN"),
            TC(address) => write!(f, "TC\t{}", address),
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
