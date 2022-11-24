use std::slice::Iter;

use crate::tokenizer::*;
use crate::sexpr::*;

pub struct Parse<'a> {
    pub modules: &'a [Module<'a>],
}

pub struct Module<'a> {
    pub name: &'a str,
    pub functions: &'a [Function<'a>],
}

pub struct Function<'a> {
    pub name: &'a str,
    pub arguments: &'a [Argument<'a>],
    pub return_data_type: Option<DataType<'a>>,
}

pub struct Argument<'a> {
    pub name: &'a str,
    pub data_type: DataType<'a>,
}

pub enum Statement<'a> {
    Definition(DefinitionStatement<'a>),
}

pub struct DefinitionStatement<'a> {
    pub var_name: &'a str,
    pub data_type: DataType<'a>,
    pub expression: Expression<'a>,
}

pub enum Expression<'a> {
    BinOpExpression {
        lhs: Box<Expression<'a>>,
        op: Operator,
        rhs: Box<Expression<'a>>,
    },
    NumberLiteralExpression {
        value: &'a str
    },
}

pub enum Operator {
    Addition,
}

pub struct DataType<'a> {
    pub name: &'a str,
}

pub struct Parser {
}

pub struct ParseError<'a> {
    pub token: Option<&'a Token<'a>>,
    pub description: String,
}

#[derive(Clone)]
pub struct TokenStream<'a> {
    iter: Iter<'a, Token<'a>>,
}

impl<'a> IntoValue<'a> for Operator {
    fn into(&self) -> Value<'a> {
        match self {
            Self::Addition => Value::Symbol("+"),
        }
    }
}

impl<'a> IntoValue<'a> for Expression<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        match self {
            Self::BinOpExpression { lhs, op, rhs } => {
                let car = Box::new(IntoValue::into(&*op));
                let cdr: Vec<Box<Value<'a>>> = [lhs, rhs].iter()
                    .map(|e| Box::new(IntoValue::into(&***e)))
                    .collect();
                Value::Cons(car, Box::new(Value::Vector(cdr)))
            },
            Self::NumberLiteralExpression { value } => {
                Value::Number(value)
            }
        }
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> TokenStream<'a> {
        TokenStream {
            iter: tokens.iter()
        }
    }
    
    fn try_consume(&mut self, expected: TokenKind) -> Result<&'a Token<'a>, ParseError<'a>> {
        match self.iter.next() {
            Some(token) if token.kind == expected => Ok(token),
            Some(token) => Err(ParseError {
                token: Some(token),
                description: format!("Expected: {:?} Found: {:?}", expected, token.kind),
            }),
            _ => Err(ParseError {
                token: None,
                description: String::from("Unexpected end of source"),
            })
        }
    }
}

impl Parser {
    pub fn new() -> Parser {
        Parser {  }
    }

   pub fn try_parse_expr<'a>(&self, stream: &mut TokenStream<'a>) -> Result<Expression<'a>, ParseError<'a>> {
        let lhs = stream.try_consume(TokenKind::Number)?;
        stream.try_consume(TokenKind::Plus)?;
        let rhs = stream.try_consume(TokenKind::Number)?;
        Ok(Expression::BinOpExpression {
            lhs: Box::new(Expression::NumberLiteralExpression { value: lhs.range }),
            op: Operator::Addition,
            rhs: Box::new(Expression::NumberLiteralExpression { value: rhs.range }),
        })
    }

    pub fn try_parse<'a>(&self, _tokens: &'a [Token<'a>]) -> Result<Parse<'a>, ParseError<'a>> {
        todo!()
    }
}
