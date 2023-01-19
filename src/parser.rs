use std::marker::PhantomData;
use std::slice::Iter;

use crate::ast::*;
use crate::tokenizer::*;

#[derive(Clone)]
pub struct TokenStream<'a> {
    iter: Iter<'a, Token<'a>>,
    offset: usize,
}

pub struct SyntaxError<'a> {
    pub position: TokenStream<'a>,
    pub context: Vec<String>,
}

type ParseResult<'a, T> = Result<(TokenStream<'a>, T), SyntaxError<'a>>;

trait Parser<'a> {
    type Value: Sized;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value>;

    fn map<U>(self, f: fn(Self::Value) -> U) -> MappedParser<'a, Self, U>
    where
        Self: Sized,
    {
        MappedParser { p: self, mapfn: f }
    }
}

struct TokenParser(TokenKind);

struct EmbeddedParser<'a, F, T>
where
    F: Fn(TokenStream<'a>) -> Result<(TokenStream<'a>, T), SyntaxError<'a>>,
{
    _phantom: PhantomData<&'a T>,
    f: F,
}

struct MappedParser<'a, P, U>
where
    P: Parser<'a>,
{
    p: P,
    mapfn: fn(P::Value) -> U,
}

// Parses using the first successful parser pprovided. If no parsers match,
// the error returned is the error from the parse that consumed the most tokens
macro_rules! parse_first {
    ($($parser:expr),*) => {
        {
            let f: fn(TokenStream<'a>) -> Result<(TokenStream<'a>, _), SyntaxError<'a>> = |stream| {
                let mut most_significant_error = SyntaxError::new(stream.clone());
                $(
                    {
                        match ($parser).try_parse_from(stream.clone()) {
                            Ok((s, value)) => return Ok((s, value)),
                            Err(e) => {
                                if e.position.offset > most_significant_error.position.offset {
                                    most_significant_error = e
                                }
                            }
                        };
                    }
                )*

                Err(most_significant_error)
            };

            EmbeddedParser { f, _phantom: PhantomData }
        }
    }
}

macro_rules! parse_optional {
    ($parser:expr) => {{
        let f: fn(TokenStream<'a>) -> Result<(TokenStream<'a>, _), SyntaxError<'a>> =
            |stream| match ($parser).try_parse_from(stream.clone()) {
                Ok((s, value)) => Ok((s, Some(value))),
                Err(e) => {
                    if e.position.offset > stream.offset {
                        Err(e)
                    } else {
                        Ok((stream, None))
                    }
                }
            };

        EmbeddedParser {
            f,
            _phantom: PhantomData,
        }
    }};
}

// Parses a non-left-recursive sequence
macro_rules! parse_sequence {
    ($($parser:expr),*) => {
        {
            let f: fn(TokenStream<'a>) -> Result<(TokenStream<'a>, _), SyntaxError<'a>> = |stream| {
                let mut s = stream.clone();
                let value = (
                    $(
                        {
                            let (s_prime, x) = ($parser).try_parse_from(s.clone())?;
                            s = s_prime;
                            x
                        },
                    )*
                );

                Ok((s, value))
            };

            EmbeddedParser { f, _phantom: PhantomData }
        }
    }
}

// Parses a left-recursive rule of the form A : A alpha | beta
macro_rules! parse_left_recursive_sequence {
    (alpha: $aparser:expr, beta: $bparser:expr) => {{
        parse_sequence!(
            ($bparser),
            EmbeddedParser {
                f: |mut stream| {
                    let mut accum: Vec<_> = Vec::new();
                    loop {
                        match ($aparser).try_parse_from(stream.clone()) {
                            Ok((s, value)) => {
                                stream = s;
                                accum.push(value);
                            }
                            _ => {
                                break;
                            }
                        };
                    }

                    Ok((stream, accum))
                },
                _phantom: PhantomData
            }
        )
    }};
}

// Repeats a parser zero of more times
macro_rules! parse_zero_or_more {
    ($parser:expr) => {{
        let f: fn(TokenStream<'a>) -> Result<_, _> = |stream| {
            parse_left_recursive_sequence!(alpha: ($parser), beta: ($parser))
                .map(|(first, mut rest)| {
                    rest.insert(0, first);
                    rest
                })
                .try_parse_from(stream.clone())
                .or_else(|e| {
                    if e.position.offset > stream.offset {
                        Err(e)
                    } else {
                        Ok((stream, Vec::new()))
                    }
                })
        };

        EmbeddedParser {
            f,
            _phantom: PhantomData,
        }
    }};
}

impl<'a, F, T> Parser<'a> for EmbeddedParser<'a, F, T>
where
    F: Fn(TokenStream<'a>) -> Result<(TokenStream<'a>, T), SyntaxError<'a>>,
{
    type Value = T;

    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        (self.f)(stream)
    }
}

impl<'a, P, U> Parser<'a> for MappedParser<'a, P, U>
where
    P: Parser<'a>,
{
    type Value = U;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        match self.p.try_parse_from(stream) {
            Ok((s, t)) => Ok((s, (self.mapfn)(t))),
            Err(e) => Err(e),
        }
    }
}

impl<'a> Parser<'a> for TokenParser {
    type Value = ();
    fn try_parse_from(&self, mut stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let position = stream.clone();
        match stream.next() {
            Some(Token { kind, .. }) if *kind == self.0 => Ok((stream, ())),
            _ => Err(SyntaxError::new(position).with_context(format!("Expected: '{}'", self.0))),
        }
    }
}

struct IdentifierParser;
struct NumberLiteralParser;
struct StringLiteralParser;
struct PrimaryExpressionParser;
struct PostfixExpressionParser;
struct AddativeExpressionParser;
struct ExpressionParser;
struct DataTypeParser;
struct StatementParser;
struct BlockParser;
struct NamedArgumentParser;
struct ArgumentListParser;
struct NamedDeclarationParser;
struct DeclarationListParser;
struct ProcedureParser;
struct ImportParser;
struct ModuleParser;
struct RootParser;

impl<'a> Parser<'a> for IdentifierParser {
    type Value = Identifier<'a>;
    fn try_parse_from(&self, mut stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let position = stream.clone();
        match stream.next() {
            Some(Token {
                kind: TokenKind::Identifier,
                range,
                ..
            }) => Ok((stream, Self::Value { name: range })),
            _ => Err(SyntaxError::new(position).with_context(format!("Expected identifier"))),
        }
    }
}

impl<'a> Parser<'a> for NumberLiteralParser {
    type Value = NumberLiteral<'a>;
    fn try_parse_from(&self, mut stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let position = stream.clone();
        match stream.next() {
            Some(Token {
                kind: TokenKind::Number,
                range,
                ..
            }) => Ok((stream, Self::Value { value: range })),
            _ => Err(SyntaxError::new(position).with_context(format!("Expected number literal"))),
        }
    }
}

impl<'a> Parser<'a> for StringLiteralParser {
    type Value = StringLiteral<'a>;
    fn try_parse_from(&self, mut stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        let position = stream.clone();
        match stream.next() {
            Some(Token {
                kind: TokenKind::StringLiteral,
                range,
                ..
            }) => Ok((stream, Self::Value { value: range })),
            _ => Err(SyntaxError::new(position).with_context(format!("Expected string literal"))),
        }
    }
}

impl<'a> Parser<'a> for PrimaryExpressionParser {
    type Value = Expression<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_first!(
            IdentifierParser.map(|identifier| Expression::from(identifier)),
            NumberLiteralParser.map(|literal| Expression::from(literal)),
            parse_sequence!(
                TokenParser(TokenKind::ParenOpen),
                ExpressionParser,
                TokenParser(TokenKind::ParenClose)
            )
            .map(|v| v.1)
        )
        .try_parse_from(stream)
    }
}

#[rustfmt::skip]
impl<'a> Parser<'a> for PostfixExpressionParser {
    type Value = Expression<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_first!(parse_left_recursive_sequence!(
            alpha:
                parse_first!(
                    parse_sequence!
                        (TokenParser(TokenKind::ParenOpen),
                        TokenParser(TokenKind::ParenClose)
                    ).map(|_| PostfixOperation::Call(ArgumentList { arguments: Vec::new() })),
                    parse_sequence!(
                        TokenParser(TokenKind::ParenOpen),
                        ArgumentListParser,
                        TokenParser(TokenKind::ParenClose)
                    ).map(|v| PostfixOperation::Call(v.1)),
                    parse_sequence!(
                        TokenParser(TokenKind::Dot),
                        IdentifierParser
                    ).map(|v| PostfixOperation::Dereference(v.1))
                ),
            beta: PrimaryExpressionParser
        )
        .map(|(mut expr, postfixes)| {
            for postfix in postfixes.into_iter() {
                expr = Expression::Postfix(PostfixExpression {
                    lhs: Box::new(expr),
                    operation: postfix,
                });
            }

            expr
        }))
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for AddativeExpressionParser {
    type Value = Expression<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        const NEXT_PARSER: PostfixExpressionParser = PostfixExpressionParser;
        parse_left_recursive_sequence!(
            alpha:
                parse_sequence!(
                    parse_first!(
                        TokenParser(TokenKind::OpAddition).map(|_| Operator::Addition),
                        TokenParser(TokenKind::OpSubtraction).map(|_| Operator::Addition)
                    ),
                    NEXT_PARSER
                ),
            beta: NEXT_PARSER
        )
        .try_parse_from(stream)
        .map(|(s, (mut expr, rest))| {
            for (op, rhs) in rest.into_iter() {
                expr = Expression::from(BinaryExpression {
                    lhs: Box::new(expr),
                    op,
                    rhs: Box::new(rhs),
                });
            }

            (s, expr)
        })
    }
}

impl<'a> Parser<'a> for ExpressionParser {
    type Value = Expression<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        AddativeExpressionParser.try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for DataTypeParser {
    type Value = DataType<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        IdentifierParser
            .map(|identifier| DataType { identifier })
            .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for StatementParser {
    type Value = Statement<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_sequence!(
            parse_first!(
                parse_sequence!(
                    TokenParser(TokenKind::KwdLet),
                    IdentifierParser,
                    TokenParser(TokenKind::Colon),
                    DataTypeParser,
                    TokenParser(TokenKind::Assignment),
                    ExpressionParser
                )
                .map(|v| Statement::Definition(DefinitionStatement {
                    identifier: v.1,
                    data_type: v.3,
                    expression: v.5
                })),
                parse_sequence!(IdentifierParser, TokenParser(TokenKind::Assignment), ExpressionParser).map(|v| {
                    Statement::Assignment(AssignmentStatement {
                        identifier: v.0,
                        expression: v.2,
                    })
                })
            ),
            TokenParser(TokenKind::SemiColon)
        )
        .map(|v| v.0)
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for BlockParser {
    type Value = Block<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_sequence!(
            TokenParser(TokenKind::BraceOpen),
            parse_zero_or_more!(StatementParser),
            TokenParser(TokenKind::BraceClose)
        )
        .map(|v| Block { statements: v.1 })
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for NamedArgumentParser {
    type Value = NamedArgument<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_sequence!(IdentifierParser, TokenParser(TokenKind::Colon), ExpressionParser)
            .map(|v| NamedArgument {
                identifier: v.0,
                expression: v.2,
            })
            .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for ArgumentListParser {
    type Value = ArgumentList<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_left_recursive_sequence!(
            alpha: parse_sequence!(
                TokenParser(TokenKind::Comma), NamedArgumentParser
            ).map(|v| v.1),
            beta: NamedArgumentParser
        )
        .map(|(first, mut rest)| {
            rest.insert(0, first);
            ArgumentList { arguments: rest }
        })
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for NamedDeclarationParser {
    type Value = NamedDeclaration<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_sequence!(IdentifierParser, TokenParser(TokenKind::Colon), DataTypeParser)
            .map(|v| NamedDeclaration {
                identifier: v.0,
                data_type: v.2,
            })
            .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for DeclarationListParser {
    type Value = DeclarationList<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_first!(
            parse_sequence!(TokenParser(TokenKind::ParenOpen), TokenParser(TokenKind::ParenClose)).map(|_| Vec::new()),
            parse_sequence!(
                TokenParser(TokenKind::ParenOpen),
                NamedDeclarationParser,
                parse_zero_or_more!(parse_sequence!(TokenParser(TokenKind::Comma), NamedDeclarationParser)),
                TokenParser(TokenKind::ParenClose)
            )
            .map(|(_, first, rest, _)| {
                let mut results = vec![first];
                for (_, next) in rest.into_iter() {
                    results.push(next)
                }

                results
            })
        )
        .map(|v| DeclarationList { declarations: v })
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for ProcedureParser {
    type Value = Procedure<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_sequence!(
            TokenParser(TokenKind::KwdProc),
            IdentifierParser,
            DeclarationListParser,
            TokenParser(TokenKind::ArrowSingle),
            DeclarationListParser,
            BlockParser
        )
        .map(|v| Procedure {
            identifier: v.1,
            parameter_list: v.2,
            return_list: v.4,
            block: v.5,
        })
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for ImportParser {
    type Value = Import<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_sequence!(
            TokenParser(TokenKind::KwdImport),
            IdentifierParser,
            parse_optional!(parse_sequence!(TokenParser(TokenKind::KwdFrom), StringLiteralParser)),
            TokenParser(TokenKind::SemiColon)
        )
        .map(|v| Import {
            identifier: v.1,
            path: v.2.map(|f| f.1),
        })
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for ModuleParser {
    type Value = Module<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_sequence!(
            TokenParser(TokenKind::KwdModule),
            IdentifierParser,
            TokenParser(TokenKind::BraceOpen),
            parse_zero_or_more!(ImportParser),
            parse_zero_or_more!(ProcedureParser),
            TokenParser(TokenKind::BraceClose)
        )
        .map(|v| Module {
            identifier: v.1,
            imports: v.3,
            procedures: v.4,
        })
        .try_parse_from(stream)
    }
}

impl<'a> Parser<'a> for RootParser {
    type Value = Root<'a>;
    fn try_parse_from(&self, stream: TokenStream<'a>) -> ParseResult<'a, Self::Value> {
        parse_zero_or_more!(ModuleParser)
            .map(|v| Root { modules: v })
            .try_parse_from(stream)
    }
}

impl<'a> SyntaxError<'a> {
    fn new(position: TokenStream<'a>) -> Self {
        Self {
            position,
            context: vec![],
        }
    }

    fn with_context(mut self, ctx: String) -> SyntaxError<'a> {
        self.context.push(ctx);
        self
    }
}

impl<'a> std::fmt::Display for SyntaxError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.position.peek() {
            Some(..) => {
                writeln!(f, "At:\n  {}", self.position.sample_source())?;
            }
            None => {
                writeln!(f, "At: <end of source>")?;
            }
        }

        writeln!(f)?;
        writeln!(f, "Syntax error:")?;
        for (i, ctx) in self.context.iter().enumerate().take(30) {
            write!(f, "  {: <1$}", "", i)?;
            writeln!(f, "{}", ctx)?;
        }

        Ok(())
    }
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> TokenStream<'a> {
        TokenStream {
            iter: tokens.iter(),
            offset: 0,
        }
    }

    fn next(&mut self) -> Option<&'a Token<'a>> {
        let n = self.iter.next();
        self.offset += 1;
        n
    }

    fn peek(&self) -> Option<&'a Token<'a>> {
        self.iter.clone().next()
    }

    #[allow(dead_code)]
    fn sample_source(&self) -> String {
        match self.iter.clone().next() {
            Some(token) => {
                let source: String = token.rest.chars().take(80).take_while(|c| *c != '\r' && *c != '\n').collect();
                format!("{}", source)
            }
            None => String::from("<^Z>"),
        }
    }
}

pub fn try_parse_from<'a>(stream: &mut TokenStream<'a>) -> ParseResult<'a, Root<'a>> {
    RootParser.try_parse_from(stream.clone())
}

pub fn try_parse_all<'a>(tokens: &'a [Token<'a>]) -> Result<Root, SyntaxError<'a>> {
    try_parse_from(&mut TokenStream::new(&tokens)).and_then(|(stream, parse)| match stream.peek() {
        Some(token) => Err(SyntaxError::new(stream).with_context(format!("Unexpected: {}", token.range))),
        None => Ok(parse),
    })
}
