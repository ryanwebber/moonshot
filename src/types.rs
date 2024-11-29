#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Numeric {
    Real(Real),
}

impl std::fmt::Display for Numeric {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Numeric::Real(real) => write!(f, "{}", real),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Real {
    pub base: i32,

    /// Base-10 exponent
    pub exponent: i32,
}

impl From<i32> for Real {
    fn from(value: i32) -> Self {
        Self {
            base: value,
            exponent: 0,
        }
    }
}

impl From<f32> for Real {
    fn from(value: f32) -> Self {
        let mut base = value;
        let mut exponent = 0;

        while base.fract() != 0.0 {
            base *= 10.0;
            exponent -= 1;
        }

        Self {
            base: base as i32,
            exponent,
        }
    }
}

impl std::fmt::Display for Real {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.base {
            0 => write!(f, "0")?,
            b if b < 0 => write!(f, "-{}", -b)?,
            b if b > 0 => write!(f, "+{}", b)?,
            _ => unreachable!(),
        };

        if self.exponent != 0 {
            write!(f, " E{}", self.exponent)?;
        }

        Ok(())
    }
}
