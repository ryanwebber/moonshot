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
    pub identifier: Identifier<'a>,
    pub data_type: DataType<'a>,
}

pub struct Block<'a> {
    pub statements: Vec<Statement<'a>>,
}

pub enum Statement<'a> {
    Assignment(AssignmentStatement<'a>),
    Definition(DefinitionStatement<'a>),
}

pub struct AssignmentStatement<'a> {
    pub identifier: Identifier<'a>,
    pub expression: Expression<'a>,
}

pub struct DefinitionStatement<'a> {
    pub identifier: Identifier<'a>,
    pub data_type: DataType<'a>,
    pub expression: Expression<'a>,
}

pub struct Identifier<'a> {
    pub name: &'a str,
}

pub struct NumberLiteral<'a> {
    pub value: &'a str,
}

pub enum Expression<'a> {
    BinOp(BinaryExpression<'a>),
    Call(CallExpression<'a>),
    Constant(NumberLiteral<'a>),
    Dereference(Identifier<'a>),
}

pub struct BinaryExpression<'a> {
    pub lhs: Box<Expression<'a>>,
    pub op: Operator,
    pub rhs: Box<Expression<'a>>,
}

pub struct CallExpression<'a> {
    pub identifier: Identifier<'a>,
}

pub enum Operator {
    Addition,
}

pub struct DataType<'a> {
    pub identifier: Identifier<'a>,
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
            Expression::BinOp(v) => sexpr::Value::from(v),
            Expression::Call(v) => sexpr::Value::from(v),
            Expression::Constant(v) => sexpr::Value::from(v),
            Expression::Dereference(v) => sexpr::Value::from(v),
        }
    }
}

impl<'a> From<BinaryExpression<'a>> for Expression<'a> {
    fn from(v: BinaryExpression<'a>) -> Self {
        Expression::BinOp(v)
    }
}

impl<'a> From<CallExpression<'a>> for Expression<'a> {
    fn from(v: CallExpression<'a>) -> Self {
        Expression::Call(v)
    }
}

impl<'a> From<NumberLiteral<'a>> for Expression<'a> {
    fn from(v: NumberLiteral<'a>) -> Self {
        Expression::Constant(v)
    }
}

impl<'a> From<Identifier<'a>> for Expression<'a> {
    fn from(v: Identifier<'a>) -> Self {
        Expression::Dereference(v)
    }
}

impl<'a> From<&BinaryExpression<'a>> for sexpr::Value {
    fn from(v: &BinaryExpression<'a>) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::from(&*(v.lhs)),
            sexpr::Value::from(&(v.op)),
            sexpr::Value::from(&*(v.rhs)),
        ])
    }
}

impl<'a> From<&CallExpression<'a>> for sexpr::Value {
    fn from(v: &CallExpression<'a>) -> Self {
        sexpr::Value::List(vec![sexpr::Value::from(&v.identifier)])
    }
}

impl<'a> From<&Identifier<'a>> for sexpr::Value {
    fn from(v: &Identifier<'a>) -> Self {
        sexpr::Value::Symbol(String::from(v.name))
    }
}

impl<'a> From<&NumberLiteral<'a>> for sexpr::Value {
    fn from(literal: &NumberLiteral) -> Self {
        sexpr::Value::Number(String::from(literal.value))
    }
}

impl<'a> From<&Statement<'a>> for sexpr::Value {
    fn from(v: &Statement<'a>) -> Self {
        match v {
            Statement::Assignment(v) => sexpr::Value::from(v),
            Statement::Definition(v) => sexpr::Value::from(v),
        }
    }
}

impl<'a> From<&AssignmentStatement<'a>> for sexpr::Value {
    fn from(v: &AssignmentStatement<'a>) -> Self {
        sexpr::Value::List(vec![sexpr::Value::from(&v.identifier), sexpr::Value::from(&v.expression)])
    }
}

impl<'a> From<&DefinitionStatement<'a>> for sexpr::Value {
    fn from(v: &DefinitionStatement<'a>) -> Self {
        sexpr::Value::List(vec![
            sexpr::Value::Symbol(String::from("define")),
            sexpr::Value::from(&v.identifier),
            sexpr::Value::from(&v.data_type),
            sexpr::Value::from(&v.expression),
        ])
    }
}

impl<'a> From<&DataType<'a>> for sexpr::Value {
    fn from(v: &DataType<'a>) -> Self {
        sexpr::Value::from(&v.identifier)
    }
}

impl<'a> From<&Block<'a>> for sexpr::Value {
    fn from(v: &Block<'a>) -> Self {
        sexpr::Value::List(v.statements.iter().map(Into::into).collect())
    }
}

impl<'a> From<&DeclarationList<'a>> for sexpr::Value {
    fn from(v: &DeclarationList<'a>) -> Self {
        sexpr::Value::List(v.declarations.iter().map(Into::into).collect())
    }
}

impl<'a> From<&NamedDeclaration<'a>> for sexpr::Value {
    fn from(v: &NamedDeclaration<'a>) -> Self {
        sexpr::Value::List(vec![sexpr::Value::from(&v.identifier), sexpr::Value::from(&v.data_type)])
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
