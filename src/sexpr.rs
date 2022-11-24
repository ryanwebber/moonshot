pub enum Value<'a> {
    Cons(Box<Value<'a>>, Box<Value<'a>>),
    Nil,
    Number(&'a str),
    Symbol(&'a str),
    Vector(Vec<Box<Value<'a>>>)
}

pub struct Printer {
}

pub trait Display {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, p: &mut Printer) -> Result<(), std::fmt::Error>;
}

pub trait IntoValue<'a> {
    fn into(&self) -> Value<'a>;
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

    fn write(&mut self, f: &mut std::fmt::Formatter<'_>, s: &str) -> Result<(), std::fmt::Error> {
        write!(f, "{}", s)
    }
}

impl<'a> Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, p: &mut Printer) -> Result<(), std::fmt::Error> {
        match self {
            Value::Cons(car, cdr) => {
                p.open(f)?;
                car.fmt(f, p)?;
                p.space(f)?;
                cdr.fmt(f, p)?;
                p.close(f)?;
            },
            Value::Nil => {
                p.write(f, "nil")?;
            }
            Value::Number(value) => {
                p.write(f, value)?;
            }
            Value::Symbol(value) => {
                p.write(f, value)?;
            }
            Value::Vector(vec) => {
                p.open(f)?;
                for (i, v) in vec.iter().enumerate() {
                    v.fmt(f, p)?;
                    if i < vec.len() - 1 {
                        p.space(f)?;
                    }
                }
                p.close(f)?;
            }
        };

        Ok(())
    }
}

impl<'a> std::fmt::Display for Value<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut p = Printer {};
        
        p.open(f)?;
        Display::fmt(self, f, &mut p)?;
        p.close(f)?;
        Ok(())
    }
}
