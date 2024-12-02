use crate::types::Numeric;

#[derive(Debug, Clone)]
pub struct ProgramFragment {
    pub directives: Vec<Directive>,
}

#[derive(Debug, Clone)]
pub enum Directive {
    Include {
        path: String,
        alias: String,
    },
    UserProgram {
        spec: Vec<SpecEntry>,
    },
    State {
        name: String,
        parameters: Vec<ValueDeclaration>,
        values: Vec<ValueDefinition>,
        body: Block,
    },
    Subroutine {
        name: String,
        parameters: Vec<ValueDeclaration>,
        body: Block,
    },
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Definition(ValueDefinition),
    Expression(Expression),
    InlineAssembly(String),
    Return(Option<Expression>),
}

#[derive(Debug, Clone)]
pub struct ValueDeclaration {
    pub identifier: String,
    pub value_type: String,
}

#[derive(Debug, Clone)]
pub struct ValueDefinition {
    pub declaration: ValueDeclaration,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum Expression {
    BinaryOperation {
        op: BinaryOperator,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    FunctionCall {
        function: ValueIdentifier,
        arguments: Vec<Argument>,
    },
    NumberLiteral(Numeric),
    VariableReference(ValueIdentifier),
}

impl Expression {
    pub fn brief(&self) -> ExpressionBrief {
        ExpressionBrief { expression: self }
    }
}

pub struct ExpressionBrief<'a> {
    expression: &'a Expression,
}

impl std::fmt::Display for ExpressionBrief<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.expression {
            Expression::BinaryOperation { op, lhs, rhs } => {
                write!(f, "({} {} {})", op, lhs.brief(), rhs.brief())
            }
            Expression::FunctionCall { function, arguments } => {
                write!(f, "{}(", function)?;
                for arg in arguments.iter() {
                    write!(f, "{}:", arg.name)?;
                }
                write!(f, ")")
            }
            Expression::NumberLiteral(n) => write!(f, "{}", n),
            Expression::VariableReference(v) => write!(f, "{}", v),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Addition,
}

impl std::fmt::Display for BinaryOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BinaryOperator::Addition => write!(f, "+"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Argument {
    pub name: String,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub struct SpecEntry {
    pub property: String,
    pub expression: Expression,
}

#[derive(Debug, Clone)]
pub enum ValueIdentifier {
    Implicit(String),
    Namespaced(String, String),
}

impl std::fmt::Display for ValueIdentifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueIdentifier::Implicit(name) => write!(f, "{}", name),
            ValueIdentifier::Namespaced(namespace, name) => write!(f, "{}::{}", namespace, name),
        }
    }
}
