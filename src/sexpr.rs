pub enum Value {
    List(Vec<Value>),
    Nil,
    Number(String),
    Symbol(String),
    String(String),
}

pub struct Printer {}

pub trait Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, p: &mut Printer) -> Result<(), std::fmt::Error>;
}

pub trait IntoValue {
    fn into(&self) -> Value;
}

impl Printer {
    fn open(&mut self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "(")?;
        Ok(())
    }

    fn close(&mut self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, ")")?;
        Ok(())
    }

    fn space(&mut self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, " ")?;
        Ok(())
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, p: &mut Printer) -> Result<(), std::fmt::Error> {
        match &self {
            Value::List(vec) => {
                p.open(f)?;
                for (i, v) in vec.iter().enumerate() {
                    v.fmt(f, p)?;
                    if i < vec.len() - 1 {
                        p.space(f)?;
                    }
                }
                p.close(f)?;
            }
            Value::Nil => {
                write!(f, "nil")?;
            }
            Value::Number(value) => {
                write!(f, "{}", value)?;
            }
            Value::String(value) => {
                write!(f, "\"{}\"", value)?;
            }
            Value::Symbol(value) => {
                write!(f, "{}", value)?;
            }
        };

        Ok(())
    }
}

impl<'a> std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut p = Printer {};

        Display::fmt(self, f, &mut p)?;
        Ok(())
    }
}
