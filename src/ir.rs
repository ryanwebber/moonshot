use crate::sexpr;

pub enum Instruction {
    Set(LVal, RVal),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
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
    pub value: ConstValue,
}

pub enum ConstValue {
    Float { data: f32, precision: Precision },
}

pub enum Precision {
    Single,
}

#[derive(Debug)]
pub enum DataMode {
    SP1,
}

impl std::fmt::Display for DataMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::SP1 => write!(f, "SP1"),
        }
    }
}

impl From<&Instruction> for sexpr::Value {
    fn from(v: &Instruction) -> Self {
        match v {
            Instruction::Set(lval, rval) => sexpr::Value::List(vec![
                sexpr::Value::Symbol(String::from("set")),
                sexpr::Value::from(lval),
                sexpr::Value::from(rval),
            ]),
        }
    }
}

impl From<&LVal> for sexpr::Value {
    fn from(v: &LVal) -> Self {
        match v {
            LVal::Reg(expr) => sexpr::Value::from(expr),
        }
    }
}

impl From<&RVal> for sexpr::Value {
    fn from(v: &RVal) -> Self {
        match v {
            RVal::Add(expr) => sexpr::Value::from(expr),
            RVal::Const(expr) => sexpr::Value::from(expr),
            RVal::Reg(expr) => sexpr::Value::from(expr),
        }
    }
}

impl From<&RegExpr> for sexpr::Value {
    fn from(v: &RegExpr) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(format!("reg:{:?}", v.mode)),
            sexpr::Value::Number(format!("{}", v.reg.0)),
        ])
    }
}

impl From<&AddExpr> for sexpr::Value {
    fn from(v: &AddExpr) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(format!("add:{:?}", v.mode)),
            Into::into(&*v.lhs),
            Into::into(&*v.rhs),
        ])
    }
}

impl From<&ConstExpr> for sexpr::Value {
    fn from(v: &ConstExpr) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(String::from("const")),
            match &v.value {
                ConstValue::Float { data, precision } => sexpr::Value::List(vec![
                    match precision {
                        Precision::Single => sexpr::Value::Symbol(DataMode::SP1.to_string()),
                    },
                    sexpr::Value::Symbol(format!("{}", data)),
                ]),
            },
        ])
    }
}
