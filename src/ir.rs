use crate::sexpr;

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Id(pub usize);

pub enum Instruction {
    Set(LVal, RVal),
}

pub enum LVal {
    Reg(RegExpr),
}

pub enum RVal {
    Reg(RegExpr),
    Add(AddExpr),
    Call(CallExpr),
    Const(ConstExpr),
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Register {
    pub id: Id,
    pub offset: usize,
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

pub struct CallExpr {
    pub id: Id,
}

pub struct ConstExpr {
    pub id: Id,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstValue {
    Float { base: i32, exponent: i32, precision: Precision },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::ops::AddAssign<usize> for Id {
    fn add_assign(&mut self, rhs: usize) {
        self.0 += rhs
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
            RVal::Call(expr) => sexpr::Value::from(expr),
            RVal::Const(expr) => sexpr::Value::from(expr),
            RVal::Reg(expr) => sexpr::Value::from(expr),
        }
    }
}

impl From<&RegExpr> for sexpr::Value {
    fn from(v: &RegExpr) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(format!("reg:{:?}", v.mode)),
            sexpr::Value::Symbol(format!("{}", v.reg.id.0)),
            sexpr::Value::Symbol(format!("+{}", v.reg.offset)),
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

impl From<&CallExpr> for sexpr::Value {
    fn from(v: &CallExpr) -> Self {
        sexpr::Value::List(vec![sexpr::Value::Symbol(String::from("call")), sexpr::Value::from(v.id)])
    }
}

impl From<&ConstExpr> for sexpr::Value {
    fn from(v: &ConstExpr) -> Self {
        sexpr::Value::List(vec![sexpr::Value::Symbol(String::from("const")), sexpr::Value::from(v.id)])
    }
}

impl From<Id> for sexpr::Value {
    fn from(id: Id) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(String::from("id")),
            sexpr::Value::Number(format!("{}", id.0)),
        ])
    }
}
