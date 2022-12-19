pub struct PrettyPrintable<'a> {
    _indent: usize,
    node: &'a Value,
}

struct Printer;

pub enum Value {
    List(Vec<Value>),
    Nil,
    Number(String),
    Symbol(String),
    String(String),
}

impl Printer {
    fn write_expr<F>(f: &mut std::fmt::Formatter<'_>, ff: F) -> Result<(), std::fmt::Error>
    where
        F: FnOnce(&mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error>,
    {
        write!(f, "(")?;
        ff(f)?;
        write!(f, ")")?;
        Ok(())
    }

    fn write_space(f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, " ")
    }

    fn write_nil(f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "nil")
    }

    fn write_number(f: &mut std::fmt::Formatter<'_>, v: &str) -> Result<(), std::fmt::Error> {
        write!(f, "{}", v)
    }

    fn write_string(f: &mut std::fmt::Formatter<'_>, v: &str) -> Result<(), std::fmt::Error> {
        write!(f, "\"{}\"", v)
    }

    fn write_symbol(f: &mut std::fmt::Formatter<'_>, v: &str) -> Result<(), std::fmt::Error> {
        write!(f, "{}", v)
    }
}

impl<'a> std::fmt::Display for PrettyPrintable<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.node.fmt(f)
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            Value::List(vec) => {
                Printer::write_expr(f, |f| {
                    for (i, v) in vec.iter().enumerate() {
                        v.fmt(f)?;
                        if i < vec.len() - 1 {
                            Printer::write_space(f)?;
                        }
                    }

                    Ok(())
                })?;
            }
            Value::Nil => {
                Printer::write_nil(f)?;
            }
            Value::Number(v) => {
                Printer::write_number(f, v)?;
            }
            Value::String(v) => {
                Printer::write_string(f, v)?;
            }
            Value::Symbol(v) => {
                Printer::write_symbol(f, v)?;
            }
        };

        Ok(())
    }
}

impl Value {
    pub fn to_pretty<'a>(&'a self) -> PrettyPrintable<'a> {
        PrettyPrintable { _indent: 0, node: self }
    }
}
