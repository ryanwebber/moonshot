use crate::sexpr;

pub enum Instruction {
    Set(LVal, RVal),
}

pub struct Register(pub usize);

pub enum LVal {
    Reg(RegExpr),
}

pub enum RVal {
    Reg(RegExpr),
    Add(AddExpr),
    Const(ConstExpr),
}

pub struct RegExpr {
    pub reg: Register,
    pub mode: DataMode,
}

pub struct AddExpr {
    pub mode: DataMode,
    pub lhs: Box<RVal>,
    pub rhs: Box<RVal>,
}

pub struct ConstExpr {
    pub mode: DataMode,
    pub value: String,
}

#[derive(Debug)]
pub enum DataMode {
    SP1,
}

impl sexpr::IntoValue for Instruction {
    fn into(&self) -> sexpr::Value {
        match self {
            Self::Set(lval, rval) => {
                sexpr::Value::List(vec![
                    sexpr::Value::Symbol(String::from("set")),
                    sexpr::IntoValue::into(lval),
                    sexpr::IntoValue::into(rval),
                ])
            }
        }
    }
}

impl sexpr::IntoValue for LVal {
    fn into(&self) -> sexpr::Value {
        match self {
            Self::Reg(expr) => {
                sexpr::IntoValue::into(expr)
            }
        }
    }
}

impl sexpr::IntoValue for RVal {
    fn into(&self) -> sexpr::Value {
        match self {
            Self::Add(expr) => {
                sexpr::IntoValue::into(expr)
            },
            Self::Const(expr) => {
                sexpr::IntoValue::into(expr)
            },
            Self::Reg(expr) => {
                sexpr::IntoValue::into(expr)
            },
        }
    }
}

impl sexpr::IntoValue for RegExpr {
    fn into(&self) -> sexpr::Value {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(format!("reg:{:?}", self.mode)),
            sexpr::Value::Number(format!("{}", self.reg.0)),
        ])
    }
}

impl sexpr::IntoValue for AddExpr {
    fn into(&self) -> sexpr::Value {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(format!("add:{:?}", self.mode)),
            sexpr::IntoValue::into(&*self.lhs),
            sexpr::IntoValue::into(&*self.rhs),
        ])
    }
}

impl sexpr::IntoValue for ConstExpr {
    fn into(&self) -> sexpr::Value {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(format!("const:{:?}", self.mode)),
            sexpr::Value::Symbol(self.value.to_string()),
        ])
    }
}
