use std::slice::Iter;

use crate::ast::*;
use crate::tokenizer::*;

macro_rules! r#inline {
    ($block:block) => {
        (|| $block)()
    };
}

pub struct Wanting<'a> {
    pub position: Option<&'a Token<'a>>,
    pub wanted: String,
}

#[derive(Debug)]
pub enum PartialParseError {
    Unmatched { wanted: String },
    Unexpected { description: String },
}

#[derive(Debug)]
pub struct SyntaxError<'a> {
    pub kind: PartialParseError,
    pub position: Option<&'a Token<'a>>,
    pub context: Vec<String>,
}

#[derive(PartialEq)]
pub enum Required {
    Yes,
    No,
}

pub type PartialParse<'a, Node> = Result<Node, SyntaxError<'a>>;

#[derive(Clone)]
pub struct TokenStream<'a> {
    iter: Iter<'a, Token<'a>>,
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
            }
            None => {
                writeln!(f, "At: <end of source>")?;
            }
        }

        match &self.kind {
            PartialParseError::Unmatched { wanted } => {
                writeln!(f, "Error: Expected {}", wanted)?;
            }
            PartialParseError::Unexpected { description } => {
                writeln!(f, "Error: {}", description)?;
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
        TokenStream { iter: tokens.iter() }
    }

    fn seek(&mut self, other: TokenStream<'a>) {
        self.iter = other.iter.clone()
    }

    fn peek(&self) -> Option<&'a Token<'a>> {
        match self.iter.clone().next() {
            Some(t) => Some(t),
            None => None,
        }
    }

    fn sample_source(&self) -> String {
        match self.iter.clone().next() {
            Some(token) => {
                let source: String = token.rest.chars().take(24).take_while(|c| *c != '\r' && *c != '\n').collect();
                format!("{}...", source)
            }
            None => String::from("<^Z>"),
        }
    }

    fn try_consume(&mut self, expected: TokenKind, required: Required) -> Result<&'a Token<'a>, SyntaxError<'a>> {
        let mut iter_clone = self.iter.clone();
        match iter_clone.next() {
            Some(token) if token.kind == expected => {
                _ = self.iter.next();
                Ok(token)
            }
            Some(token) => {
                if required == Required::Yes {
                    Err(SyntaxError {
                        kind: PartialParseError::Unexpected {
                            description: format!("Unexpected: '{}' (Did you mean: '{}'?)", token.range, expected),
                        },
                        position: Some(token),
                        context: vec![],
                    })
                } else {
                    Err(SyntaxError {
                        kind: PartialParseError::Unmatched {
                            wanted: format!("{:?}", expected),
                        },
                        position: Some(token),
                        context: vec![],
                    })
                }
            }
            _ => Err(SyntaxError {
                kind: PartialParseError::Unexpected {
                    description: format!("Unexpected end of source. Expected: '{}'", expected),
                },
                position: None,
                context: vec![],
            }),
        }
    }
}

fn void_type<'a>() -> DataType<'a> {
    DataType { name: "void" }
}

fn try_parse_zero_or_more<'a, T>(
    stream: &mut TokenStream<'a>,
    separator: Option<TokenKind>,
    f: fn(&mut TokenStream<'a>) -> PartialParse<'a, T>,
) -> PartialParse<'a, Vec<T>> {
    let mut nodes: Vec<T> = Vec::new();
    loop {
        let mut stream_clone = stream.clone();
        if stream_clone.peek().is_none() {
            return Ok(nodes);
        }

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
                SyntaxError {
                    kind: PartialParseError::Unexpected { .. },
                    ..
                } => {
                    return Err(err);
                }
                SyntaxError {
                    kind: PartialParseError::Unmatched { .. },
                    ..
                } => {
                    return Ok(nodes);
                }
            },
        };
    }
}

fn try_parse_binary_op_expr<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
    stream.try_consume(TokenKind::ParenOpen, Required::No)?;
    let lhs = try_parse_expr(stream)?;
    stream.try_consume(TokenKind::OpAddition, Required::Yes)?;
    let rhs = try_parse_expr(stream)?;
    stream.try_consume(TokenKind::ParenClose, Required::Yes)?;
    Ok(Expression::BinOpExpression {
        lhs: Box::new(lhs),
        op: Operator::Addition,
        rhs: Box::new(rhs),
    })
}

fn try_parse_var_access<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
    let base_identifier = stream.try_consume(TokenKind::Identifier, Required::No)?;
    let mut path: Vec<&'a str> = vec![base_identifier.range];

    // TODO: This doesn't create a proper AST representation for tuples
    loop {
        match stream.peek() {
            Some(t) if t.kind == TokenKind::Dot => {
                let next_identifier = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
                path.push(next_identifier.range);
            }
            _ => {
                break;
            }
        };
    }

    if path.len() == 1 {
        path.push("_")
    }

    Ok(Expression::VarAccess { path })
}

fn try_parse_numeric_literal_expr<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
    let num_token = stream.try_consume(TokenKind::Number, Required::No)?;
    Ok(Expression::NumberLiteralExpression { value: num_token.range })
}

pub fn try_parse_expr<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Expression<'a>> {
    let initial_stream = stream.clone();
    let parsers = [try_parse_binary_op_expr, try_parse_numeric_literal_expr, try_parse_var_access];

    parsers
        .iter()
        .map(|&p| {
            p(stream).map_err(|op| op.with_context(format!("While parsing expression: {}", initial_stream.sample_source())))
        })
        .find(|r| match r {
            Err(SyntaxError {
                kind: PartialParseError::Unmatched { .. },
                ..
            }) => false,
            _ => true,
        })
        .unwrap_or(Err(SyntaxError {
            kind: PartialParseError::Unexpected {
                description: format!("Unable to parse expression"),
            },
            position: stream.peek(),
            context: vec![],
        }
        .with_context(format!("While parsing expression: {}", initial_stream.sample_source()))))
}

pub fn try_parse_data_type<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, DataType<'a>> {
    let type_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
    return Ok(DataType { name: type_name.range });
}

pub fn try_parse_statement<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Statement<'a>> {
    let initial_stream = stream.clone();
    inline!({
        stream.try_consume(TokenKind::KwdLet, Required::No)?;
        let var_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
        stream.try_consume(TokenKind::Colon, Required::Yes)?;
        let data_type = try_parse_data_type(stream)?;
        stream.try_consume(TokenKind::Assignment, Required::Yes)?;
        let expression = try_parse_expr(stream)?;
        stream.try_consume(TokenKind::SemiColon, Required::Yes)?;
        Ok(Statement::Definition {
            name: var_name.range,
            data_type,
            expression,
        })
    })
    .map_err(|op: SyntaxError<'a>| op.with_context(format!("While parsing statement: {}", initial_stream.sample_source())))
}

pub fn try_parse_block<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Block<'a>> {
    stream.try_consume(TokenKind::BraceOpen, Required::No)?;
    let statements = try_parse_zero_or_more(stream, None, try_parse_statement)?;
    stream.try_consume(TokenKind::BraceClose, Required::Yes)?;
    Ok(Block { statements })
}

pub fn try_parse_named_parameter<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, NamedParameter<'a>> {
    let elem_name = stream.try_consume(TokenKind::Identifier, Required::No)?;
    stream.try_consume(TokenKind::Colon, Required::Yes)?;
    let elem_type = try_parse_data_type(stream)?;
    Ok(NamedParameter {
        name: elem_name.range,
        data_type: elem_type,
    })
}

pub fn try_parse_parameter_list<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, ParameterList<'a>> {
    stream.try_consume(TokenKind::ParenOpen, Required::No)?;
    let parameters = try_parse_zero_or_more(stream, Some(TokenKind::Comma), try_parse_named_parameter)?;
    stream.try_consume(TokenKind::ParenClose, Required::Yes)?;
    Ok(ParameterList { parameters })
}

pub fn try_parse_import<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Import<'a>> {
    stream.try_consume(TokenKind::KwdImport, Required::No)?;
    let import_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
    let mut import_path: Option<String> = None;
    if let Some(Token {
        kind: TokenKind::KwdFrom,
        ..
    }) = stream.peek()
    {
        stream.try_consume(TokenKind::KwdFrom, Required::Yes)?;
        let raw_path = stream.try_consume(TokenKind::StringLiteral, Required::Yes)?.range;
        match StringNormalizer::extract_and_unescape(raw_path) {
            Ok(path_string) => {
                import_path = Some(path_string);
            }
            Err(e) => {
                return Err(SyntaxError {
                    kind: PartialParseError::Unexpected {
                        description: e.to_string(),
                    },
                    position: stream.peek(),
                    context: vec![String::from("While parsing import path")],
                })
            }
        }
    }

    stream.try_consume(TokenKind::SemiColon, Required::Yes)?;

    Ok(Import {
        name: import_name.range,
        path: import_path,
    })
}

pub fn try_parse_proc<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Procedure<'a>> {
    let initial_stream = stream.clone();
    inline!({
        stream.try_consume(TokenKind::KwdProc, Required::No)?;
        let proc_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
        let parameter_list = try_parse_parameter_list(stream)?;

        let return_type = match stream.try_consume(TokenKind::ArrowSingle, Required::No) {
            Ok(..) => try_parse_data_type(stream)?,
            _ => void_type(),
        };

        let block = try_parse_block(stream)?;
        Ok(Procedure {
            name: proc_name.range,
            parameter_list,
            return_type,
            block,
        })
    })
    .map_err(|op: SyntaxError<'a>| op.with_context(format!("While parsing function: {}", initial_stream.sample_source())))
}

pub fn try_parse_module<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Module<'a>> {
    stream.try_consume(TokenKind::KwdModule, Required::No)?;
    let mod_name = stream.try_consume(TokenKind::Identifier, Required::Yes)?;
    stream.try_consume(TokenKind::BraceOpen, Required::Yes)?;
    let imports = try_parse_zero_or_more(stream, None, try_parse_import)?;
    let procs = try_parse_zero_or_more(stream, None, try_parse_proc)?;
    stream.try_consume(TokenKind::BraceClose, Required::Yes)?;

    Ok(Module {
        name: mod_name.range,
        imports,
        procs,
    })
}

pub fn try_parse<'a>(stream: &mut TokenStream<'a>) -> PartialParse<'a, Parse<'a>> {
    try_parse_zero_or_more(stream, None, try_parse_module).map(|mods| Parse { modules: mods })
}

pub fn try_parse_all<'a>(tokens: &'a [Token<'a>]) -> PartialParse<'a, Parse<'a>> {
    try_parse(&mut TokenStream::new(&tokens))
}
