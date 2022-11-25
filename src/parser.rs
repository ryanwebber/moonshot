use std::slice::Iter;

use crate::tokenizer::*;
use crate::sexpr::*;

macro_rules! r#inline {
    ($block:block) => {
        (|| {
            $block
        })()
    }
}

pub struct Parse<'a> {
    pub modules: Vec<Module<'a>>,
}

pub struct Module<'a> {
    pub name: &'a str,
    pub functions: Vec<Procedure<'a>>,
}

pub struct Procedure<'a> {
    pub name: &'a str,
    pub input_tuple: Tuple<'a>,
    pub return_tuple: Tuple<'a>,
    pub block: Block<'a>,
}

pub struct Tuple<'a> {
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
        data_type: DataType<'a>,
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
    fn into(&self) -> crate::sexpr::Value<'a> {
        match self {
            Self::Addition => Value::Symbol("+"),
        }
    }
}

impl<'a> IntoValue<'a> for Expression<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        match self {
            Self::BinOpExpression { lhs, op, rhs } => {
                Value::List(vec![
                    IntoValue::into(&*op),
                    IntoValue::into(&**lhs),
                    IntoValue::into(&**rhs),
                ])
            },
            Self::NumberLiteralExpression { value } => {
                Value::Number(value)
            }
        }
    }
}

impl<'a> IntoValue<'a> for Statement<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        match self {
            Self::Definition { name, data_type, expression } => Value::List(vec![
                Value::Symbol("defn"),
                Value::List(vec![
                    Value::Symbol("name"),
                    Value::Symbol(name),
                ]),
                IntoValue::into(data_type),
                IntoValue::into(expression),
            ]),
        }
    }
}

impl<'a> IntoValue<'a> for DataType<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        Value::List(vec![
            Value::Symbol("type"),
            Value::Symbol(self.name),
        ])
    }
}

impl<'a> IntoValue<'a> for Block<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        Value::List(self.statements.iter().map(|s| IntoValue::into(s)).collect())
    }
}

impl<'a> IntoValue<'a> for NamedElement<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        Value::List(vec![
            Value::List(vec![
                Value::Symbol("name"),
                Value::Symbol(self.name),
            ]),
            IntoValue::into(&self.data_type)
        ])
    }
}

impl<'a> IntoValue<'a> for Tuple<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        Value::List(self.elements.iter().map(|e| IntoValue::into(e)).collect())
    }
}

impl<'a> IntoValue<'a> for Procedure<'a> {
    fn into(&self) -> crate::sexpr::Value<'a> {
        Value::List(vec![
            Value::Symbol("proc"),
            Value::List(vec![
                Value::Symbol("name"),
                Value::Symbol(self.name),
            ]),
            IntoValue::into(&self.input_tuple),
            IntoValue::into(&self.return_tuple),
            IntoValue::into(&self.block),
        ])
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

    fn seek(&mut self, other: TokenStream<'a>) {
        self.iter = other.iter.clone()
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
                let source: String = token.rest.chars()
                    .take(24)
                    .take_while(|c| *c != '\r' && *c != '\n')
                    .collect();
                format!("{}...", source)
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

    fn try_parse_zero_or_more<'a, T>(stream: &mut TokenStream<'a>, separator: Option<TokenKind>, f: fn(&mut TokenStream<'a>) -> PartialParse<'a, T>) -> PartialParse<'a, Vec<T>> {
        let mut nodes: Vec<T> = Vec::new();
        loop {
            let mut stream_clone = stream.clone();
            let node_partial = f(&mut stream_clone);
            match node_partial {
                Ok(node) => {
                    nodes.push(node);
                    stream.seek(stream_clone);
                    match (separator, stream.peek()) {
                        (Some(separator), Some(token)) if token.kind == separator => {
                            _ = stream.iter.next();
                        }
                        _ => {}
                    }
                }
                Err(err) => match err {
                    SyntaxError { kind: PartialParseError::Unexpected { .. }, .. } => {
                        return Err(err);
                    }
                    SyntaxError { kind: PartialParseError::Unmatched { .. }, .. } => {
                        return Ok(nodes);
                    }
                }
            };
        }
    }

    fn try_parse_binary_op_expr<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
        stream.try_consume(TokenKind::ParenOpen, Required::No)?;
        let lhs = Self::try_parse_expr(stream)?;
        stream.try_consume(TokenKind::OpAddition, Required::Yes)?;
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
                context: vec!()
            }.with_context(format!("While parsing expression: {}", initial_stream.sample_source()))))
    }

    pub fn try_parse_data_type<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, DataType<'a>> {
        let type_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
        return Ok(DataType { name: type_name.range })
    }

    pub fn try_parse_statement<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Statement<'a>> {
        let initial_stream = stream.clone();
        inline!({
            stream.try_consume(TokenKind::KwdLet, Required::No)?;
            let var_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
            stream.try_consume(TokenKind::Colon, Required::Yes)?;
            let var_type = Self::try_parse_data_type(stream)?;
            stream.try_consume(TokenKind::Assignment, Required::Yes)?;
            let expr = Self::try_parse_expr(stream)?;
            stream.try_consume(TokenKind::SemiColon, Required::Yes)?;
            Ok(Statement::Definition {
                name: var_name.range,
                data_type: var_type,
                expression: expr,
            })
        })
        .map_err(|op: SyntaxError<'a> | op.with_context(format!("While parsing statement: {}", initial_stream.sample_source())))
    }

    pub fn try_parse_block<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Block<'a>> {
        stream.try_consume(TokenKind::BraceOpen, Required::No)?;
        let statements = Self::try_parse_zero_or_more(stream, None, Self::try_parse_statement)?;
        stream.try_consume(TokenKind::BraceClose, Required::Yes)?;
        Ok(Block { statements })
    }

    pub fn try_parse_named_element<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, NamedElement<'a>> {
        let elem_name = stream.try_consume(TokenKind::Identifier, Required::No)?;
        stream.try_consume(TokenKind::Colon, Required::Yes)?;
        let elem_type = Self::try_parse_data_type(stream)?;
        Ok(NamedElement {
            name: elem_name.range,
            data_type: elem_type
        })
    }

    pub fn try_parse_tuple<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Tuple<'a>> {
        stream.try_consume(TokenKind::ParenOpen, Required::No)?;
        let elements = Self::try_parse_zero_or_more(stream, Some(TokenKind::Comma), Self::try_parse_named_element)?;
        stream.try_consume(TokenKind::ParenClose, Required::No)?;
        Ok(Tuple { elements: elements })
    }

    pub fn try_parse_proc<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Procedure<'a>> {
        let initial_stream = stream.clone();
        inline!({
            stream.try_consume(TokenKind::KwdProc, Required::No)?;
            let proc_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
            let input_tuple = Self::try_parse_tuple(stream)?;
            stream.try_consume(TokenKind::ArrowSingle, Required::Yes)?;
            let return_tuple = Self::try_parse_tuple(stream)?;
            let block = Self::try_parse_block(stream)?;
            Ok(Procedure {
                name: proc_name.range,
                input_tuple,
                return_tuple,
                block,
            })
        })
        .map_err(|op: SyntaxError<'a> | op.with_context(format!("While parsing function: {}", initial_stream.sample_source())))
    }
}
