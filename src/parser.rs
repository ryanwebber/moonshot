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

pub struct Wanting<'a> {
    pub position: Option<&'a Token<'a>>,
    pub wanted: String,
}

pub enum PartialParseError {
    Unmatched { wanted: String },
    Unexpected { description: String },
}

pub struct SyntaxError<'a> {
    pub kind: PartialParseError,
    pub position: Option<&'a Token<'a>>,
    pub context: Vec<String>,
}

#[derive(PartialEq)]
pub enum Required {
    Yes, No
}

pub type PartialParse<'a, Node> = Result<Node, SyntaxError<'a>>;

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

impl<'a> SyntaxError<'a> {
    fn with_context(mut self, ctx: String) -> SyntaxError<'a> {
        self.context.push(ctx);
        self
    }
}

impl<'a> std::fmt::Display for SyntaxError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {

        match &self.position {
            Some(token) => {
                writeln!(f, "At: {}", token.range)?;
            },
            None => {
                writeln!(f, "At: <end of source>")?;
            }
        }

        match &self.kind {
            PartialParseError::Unmatched { wanted } => {
                writeln!(f, "Expected: {}", wanted)?;
            }
            PartialParseError::Unexpected { description } => {
                writeln!(f, "{}", description)?;
            }
        };

        for (i, ctx) in self.context.iter().enumerate().take(10) {
            write!(f, "  {: <1$}", "", i)?;
            writeln!(f, "{}", ctx)?;
        }

        Ok(())
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> TokenStream<'a> {
        TokenStream {
            iter: tokens.iter()
        }
    }

    fn peek(&self) -> Option<&'a Token<'a>> {
        match self.iter.clone().next() {
            Some(t) => Some(t),
            None => None
        }
    }

    fn sample_source(&self) -> String {
        match self.iter.clone().next() {
            Some(token) => {
                token.rest.chars()
                    .take(24)
                    .take_while(|c| *c != '\r' && *c != '\n')
                    .collect()
            },
            None => String::from("<^Z>")
        }
    }
    
    fn try_consume(&mut self, expected: TokenKind, required: Required) -> Result<&'a Token<'a>, SyntaxError<'a>> {
        let mut iter_clone = self.iter.clone();
        match iter_clone.next() {
            Some(token) if token.kind == expected => {
                _ = self.iter.next();
                Ok(token)
            },
            Some(token) => {
                if required == Required::Yes {
                    Err(SyntaxError {
                        kind: PartialParseError::Unexpected {
                            description: format!("Expected: {:?}, but found: {}", expected, token.range),
                        },
                        position: Some(token),
                        context: vec!(),
                    })
                } else {
                    Err(SyntaxError {
                        kind: PartialParseError::Unmatched {
                            wanted: format!("{:?}", expected),
                        },
                        position: Some(token),
                        context: vec!(),
                    })
                }
            },
            _ => Err(SyntaxError {
                kind: PartialParseError::Unexpected {
                    description: format!("Expected: {:?}, but reached the end of source", expected),
                },
                position: None,
                context: vec!(),
            })
        }
    }
}

impl Parser {
    fn try_parse_binary_op_expr<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
        stream.try_consume(TokenKind::ParenOpen, Required::No)?;
        let lhs = Self::try_parse_expr(stream)?;
        stream.try_consume(TokenKind::Plus, Required::Yes)?;
        let rhs = Self::try_parse_expr(stream)?;
        stream.try_consume(TokenKind::ParenClose, Required::Yes)?;
        Ok(Expression::BinOpExpression {
            lhs: Box::new(lhs),
            op: Operator::Addition,
            rhs: Box::new(rhs),
        })
    }

    fn try_parse_numeric_literal_expr<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
        let num_token = stream.try_consume(TokenKind::Number, Required::No)?;
        Ok(Expression::NumberLiteralExpression {
            value: num_token.range
        })
    }

    pub fn try_parse_expr<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
        let initial_stream = stream.clone();
        let parsers = [
            Self::try_parse_binary_op_expr,
            Self::try_parse_numeric_literal_expr,
        ];

        parsers.iter()
            .map(|&p| p(stream)
                .map_err(|op| op.with_context(format!("While parsing expression: {}", initial_stream.sample_source()))))
            .find(|r| match r {
                Err(SyntaxError { kind: PartialParseError::Unmatched { .. }, .. }) => false,
                _ => true
            })
            .unwrap_or(Err(SyntaxError {
                kind: PartialParseError::Unmatched {
                    wanted: format!("expression")
                },
                position: stream.peek(),
                context: vec!(format!("While parsing expression"))
            }))
    }
}
