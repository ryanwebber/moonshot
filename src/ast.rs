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
    FunctionCall {
        function: ValueIdentifier,
        arguments: Vec<Argument>,
    },
    NumberLiteral(Numeric),
    StringLiteral(String),
    VariableReference(ValueIdentifier),
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
