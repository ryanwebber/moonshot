use crate::sexpr;

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
    pub parameter_list: DeclarationList<'a>,
    pub return_list: DeclarationList<'a>,
    pub block: Block<'a>,
}

pub struct DeclarationList<'a> {
    pub declarations: Vec<NamedDeclaration<'a>>,
}

pub struct NamedDeclaration<'a> {
    pub name: &'a str,
    pub data_type: DataType<'a>,
}

pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}

pub enum Statement<'a> {
    Definition {
        declaration: NamedDeclaration<'a>,
        expression: Expression<'a>,
    },
    Assignment {
        var_name: &'a str,
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
    VarAccess {
        path: Vec<&'a str>,
    },
}

pub enum Operator {
    Addition,
}

pub struct DataType<'a> {
    pub name: &'a str,
}

impl From<&Operator> for sexpr::Value {
    fn from(v: &Operator) -> Self {
        match v {
            Operator::Addition => sexpr::Value::Symbol(String::from("+")),
        }
    }
}

impl<'a> From<&Expression<'a>> for sexpr::Value {
    fn from(v: &Expression<'a>) -> Self {
        match v {
            Expression::BinOpExpression { lhs, op, rhs } => {
                sexpr::Value::List(vec![Into::into(&*op), Into::into(&**lhs), Into::into(&**rhs)])
            }
            Expression::NumberLiteralExpression { value } => sexpr::Value::Number(String::from(*value)),
            Expression::VarAccess { path } => {
                sexpr::Value::List(path.iter().map(|p| sexpr::Value::Symbol(String::from(*p))).collect())
            }
        }
    }
}

impl<'a> From<&Statement<'a>> for sexpr::Value {
    fn from(v: &Statement<'a>) -> Self {
        match v {
            Statement::Definition { declaration, expression } => sexpr::Value::List(vec![
                sexpr::Value::Symbol(String::from("defn")),
                sexpr::Value::from(declaration),
                sexpr::Value::from(expression),
            ]),
            Statement::Assignment { var_name, expression } => sexpr::Value::List(vec![
                sexpr::Value::Symbol(String::from("assign")),
                sexpr::Value::Symbol(String::from(*var_name)),
                sexpr::Value::from(expression),
            ]),
        }
    }
}

impl<'a> From<&DataType<'a>> for sexpr::Value {
    fn from(v: &DataType<'a>) -> Self {
        sexpr::Value::Symbol(String::from(v.name))
    }
}

impl<'a> From<&Block<'a>> for sexpr::Value {
    fn from(v: &Block<'a>) -> Self {
        sexpr::Value::List(v.statements.iter().map(Into::into).collect())
    }
}

impl<'a> From<&NamedDeclaration<'a>> for sexpr::Value {
    fn from(v: &NamedDeclaration<'a>) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(String::from(v.name)),
            sexpr::Value::from(&v.data_type),
        ])
    }
}

impl<'a> From<&DeclarationList<'a>> for sexpr::Value {
    fn from(v: &DeclarationList<'a>) -> Self {
        sexpr::Value::List(v.declarations.iter().map(Into::into).collect())
    }
}

impl<'a> From<&Import<'a>> for sexpr::Value {
    fn from(v: &Import<'a>) -> Self {
        let mut list = vec![
            sexpr::Value::Symbol(String::from("import")),
            sexpr::Value::Symbol(String::from(v.name)),
        ];

        if let Some(path) = &v.path {
            list.push(sexpr::Value::String(String::from(path)));
        }

        sexpr::Value::List(list)
    }
}

impl<'a> From<&Procedure<'a>> for sexpr::Value {
    fn from(v: &Procedure<'a>) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(String::from("proc")),
            sexpr::Value::Symbol(String::from(v.name)),
            sexpr::Value::from(&v.parameter_list),
            sexpr::Value::from(&v.return_list),
            sexpr::Value::from(&v.block),
        ])
    }
}

impl<'a> From<&Module<'a>> for sexpr::Value {
    fn from(v: &Module<'a>) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(String::from("module")),
            sexpr::Value::Symbol(String::from(v.name)),
            sexpr::Value::List(v.imports.iter().map(Into::into).collect()),
            sexpr::Value::List(v.procs.iter().map(Into::into).collect()),
        ])
    }
}

impl<'a> From<&Parse<'a>> for sexpr::Value {
    fn from(v: &Parse<'a>) -> Self {
        sexpr::Value::List(v.modules.iter().map(Into::into).collect())
    }
}
