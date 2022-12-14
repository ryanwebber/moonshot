pub enum Instruction {
    AD,
    ADS,
    CAE,
    CAF,
    DEC,
    NOOP,
    RETURN,
    TC,
    TCA,
    TS,
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AD => write!(f, "AD"),
            Self::ADS => write!(f, "ADS"),
            Self::CAE => write!(f, "CAE"),
            Self::CAF => write!(f, "CAF"),
            Self::DEC => write!(f, "DEC"),
            Self::NOOP => write!(f, "NOOP"),
            Self::RETURN => write!(f, "RETURN"),
            Self::TC => write!(f, "TC"),
            Self::TCA => write!(f, "TCA"),
            Self::TS => write!(f, "TS"),
        }
    }
}
