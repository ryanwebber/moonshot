use crate::sexpr::*;

pub struct Parse<'a> {
    pub modules: Vec<Module<'a>>,
}

pub struct Module<'a> {
    pub name: &'a str,
    pub imports: Vec<Import<'a>>,
    pub procs: Vec<Procedure<'a>>,
}

pub struct Import<'a> {
    pub name: &'a str,
    pub path: Option<String>,
}

pub struct Procedure<'a> {
    pub name: &'a str,
    pub input_defn: TypeContainer<'a>,
    pub return_defn: TypeContainer<'a>,
    pub block: Block<'a>,
}

pub struct TypeContainer<'a> {
    pub elements: Vec<NamedElement<'a>>,
}

pub struct NamedElement<'a> {
    pub name: &'a str,
    pub data_type: DataType<'a>,
}

pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}

pub enum Statement<'a> {
    Definition {
        name: &'a str,
        type_container: TypeContainer<'a>,
        expression: Expression<'a>,
    },
}

pub enum Expression<'a> {
    BinOpExpression {
        lhs: Box<Expression<'a>>,
        op: Operator,
        rhs: Box<Expression<'a>>,
    },
    NumberLiteralExpression {
        value: &'a str,
    },
}

pub enum Operator {
    Addition,
}

pub struct DataType<'a> {
    pub name: &'a str,
}

impl<'a> IntoValue for Operator {
    fn into(&self) -> crate::sexpr::Value {
        match self {
            Self::Addition => Value::Symbol(String::from("+")),
        }
    }
}

impl<'a> IntoValue for Expression<'a> {
    fn into(&self) -> crate::sexpr::Value {
        match self {
            Self::BinOpExpression { lhs, op, rhs } => Value::List(vec![
                IntoValue::into(&*op),
                IntoValue::into(&**lhs),
                IntoValue::into(&**rhs),
            ]),
            Self::NumberLiteralExpression { value } => Value::Number(String::from(*value)),
        }
    }
}

impl<'a> IntoValue for Statement<'a> {
    fn into(&self) -> crate::sexpr::Value {
        match self {
            Self::Definition {
                name,
                type_container,
                expression,
            } => Value::List(vec![
                Value::Symbol(String::from("defn")),
                Value::Symbol(String::from(*name)),
                IntoValue::into(type_container),
                IntoValue::into(expression),
            ]),
        }
    }
}

impl<'a> IntoValue for DataType<'a> {
    fn into(&self) -> crate::sexpr::Value {
        Value::Symbol(String::from(self.name))
    }
}

impl<'a> IntoValue for Block<'a> {
    fn into(&self) -> crate::sexpr::Value {
        Value::List(self.statements.iter().map(IntoValue::into).collect())
    }
}

impl<'a> IntoValue for NamedElement<'a> {
    fn into(&self) -> crate::sexpr::Value {
        Value::List(vec![
            Value::Symbol(String::from(self.name)),
            IntoValue::into(&self.data_type),
        ])
    }
}

impl<'a> IntoValue for TypeContainer<'a> {
    fn into(&self) -> crate::sexpr::Value {
        Value::List(self.elements.iter().map(IntoValue::into).collect())
    }
}

impl<'a> IntoValue for Import<'a> {
    fn into(&self) -> crate::sexpr::Value {
        let mut list = vec![Value::Symbol(String::from("import")), Value::Symbol(String::from(self.name))];

        if let Some(path) = &self.path {
            list.push(Value::String(String::from(path)));
        }

        Value::List(list)
    }
}

impl<'a> IntoValue for Procedure<'a> {
    fn into(&self) -> crate::sexpr::Value {
        Value::List(vec![
            Value::Symbol(String::from("proc")),
            Value::Symbol(String::from(self.name)),
            IntoValue::into(&self.input_defn),
            IntoValue::into(&self.return_defn),
            IntoValue::into(&self.block),
        ])
    }
}

impl<'a> IntoValue for Module<'a> {
    fn into(&self) -> crate::sexpr::Value {
        Value::List(vec![
            Value::Symbol(String::from("module")),
            Value::Symbol(String::from(self.name)),
            Value::List(self.imports.iter().map(IntoValue::into).collect()),
            Value::List(self.procs.iter().map(IntoValue::into).collect()),
        ])
    }
}

impl<'a> IntoValue for Parse<'a> {
    fn into(&self) -> crate::sexpr::Value {
        Value::List(self.modules.iter().map(IntoValue::into).collect())
    }
}
